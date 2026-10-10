;; -*- lexical-binding: t -*-

;;; org-glance-version.el --- Immutable headline version DAG

(require 'cl-lib)
(require 'f)
(require 'json)
(require 'subr-x)

(cl-defstruct (org-glance-version (:predicate org-glance-version?)
                                  (:conc-name org-glance-version:))
  schema headline id kind parents targets observed-leaves content-sha256
  created producer directory valid)

(cl-defun org-glance-version:path (headline-dir version-id)
  "Return VERSION-ID's immutable directory below HEADLINE-DIR."
  (f-join headline-dir "versions" version-id))

(cl-defun org-glance-version:data-file (headline-dir version-id)
  "Return VERSION-ID's snapshot file below HEADLINE-DIR."
  (f-join (org-glance-version:path headline-dir version-id) "data.org"))

(cl-defun org-glance-version:legacy-id (headline digest)
  "Return HEADLINE and DIGEST's deterministic UUIDv5 migration id."
  (let* ((namespace (apply #'unibyte-string
                           '(#x6b #xa7 #xb8 #x10 #x9d #xad #x11 #xd1
                             #x80 #xb4 #x00 #xc0 #x4f #xd4 #x30 #xc8)))
         (hex (secure-hash 'sha1
                           (concat namespace (encode-coding-string headline 'utf-8)
                                   "\0" (encode-coding-string digest 'utf-8))))
         (bytes (substring hex 0 32)))
    (org-glance-version--uuid (org-glance-version--stamp bytes ?5))))

(cl-defun org-glance-version--uuid-v7 ()
  "Return a UUIDv7 string."
  (let* ((millis (floor (* 1000 (float-time))))
         (time-hex (format "%012x" millis))
         (random-hex (apply #'concat
                            (cl-loop repeat 10 collect (format "%02x" (random 256))))))
    (org-glance-version--uuid
     (org-glance-version--stamp (concat time-hex random-hex) ?7))))

(cl-defun org-glance-version--stamp (hex version)
  "Set UUID VERSION and variant bits in the first 32 characters of HEX."
  (let ((out (copy-sequence (substring hex 0 32))))
    (aset out 12 version)
    (aset out 16 (aref "89ab" (% (string-to-number (substring out 16 17) 16) 4)))
    out))

(cl-defun org-glance-version--uuid (hex)
  "Format 32 hexadecimal characters HEX as a UUID."
  (format "%s-%s-%s-%s-%s" (substring hex 0 8) (substring hex 8 12)
          (substring hex 12 16) (substring hex 16 20) (substring hex 20 32)))

(cl-defun org-glance-version--plist (version)
  "Return VERSION's portable metadata plist."
  (list :version (org-glance-version:schema version)
        :headline (org-glance-version:headline version)
        :id (org-glance-version:id version)
        :kind (symbol-name (org-glance-version:kind version))
        :parents (apply #'vector (org-glance-version:parents version))
        :contentSha256 (org-glance-version:content-sha256 version)
        :created (org-glance-version:created version)
        :producer (org-glance-version:producer version)))

(cl-defun org-glance-version--valid-content-p (version)
  "Return non-nil when VERSION's payload agrees with its envelope."
  (condition-case nil
      (let ((data (f-join (org-glance-version:directory version) "data.org"))
            (kind (org-glance-version:kind version))
            (digest (org-glance-version:content-sha256 version)))
        (cond
         ((eq kind 'tombstone) (and (null digest) (not (f-exists? data))))
         ((eq kind 'rejection) (and (null digest) (not (f-exists? data))))
         ((eq kind 'snapshot)
          (and digest (f-file? data)
               (equal digest (secure-hash 'sha256 (f-read-text data 'utf-8)))))
         (t nil)))
    (error nil)))

(cl-defun org-glance-version--distinct-p (values)
  (= (length values) (length (delete-dups (copy-sequence values)))))

(cl-defun org-glance-version--strings-p (values)
  (and (listp values) (cl-every #'stringp values)))

(cl-defun org-glance-version--version-event-p (version)
  (memq (org-glance-version:kind version) '(snapshot tombstone)))

(cl-defun org-glance-version--candidate (dir headline raw)
  "Decode structurally local RAW metadata at DIR for HEADLINE."
  (condition-case nil
      (let* ((schema (plist-get raw :version))
             (id (plist-get raw :id))
             (kind-name (if (= schema 1)
                            (plist-get raw :kind)
                          (plist-get raw :event)))
             (kind (pcase kind-name
                     ((or "snapshot" "snapshot-published") 'snapshot)
                     ((or "tombstone" "tombstone-published") 'tombstone)
                     ("leaves-rejected" 'rejection)))
             (parents (if (eq kind 'rejection) nil (plist-get raw :parents)))
             (targets (and (eq kind 'rejection) (plist-get raw :targets)))
             (observed (and (eq kind 'rejection)
                            (plist-get raw :observedLeaves)))
             (digest (plist-get raw :contentSha256))
             (version (make-org-glance-version
                       :schema schema :headline headline :id id :kind kind
                       :parents parents :targets targets :observed-leaves observed
                       :content-sha256 digest :created (plist-get raw :created)
                       :producer (plist-get raw :producer) :directory dir)))
        (when (and (memq schema '(1 2)) kind (stringp id)
                   (equal headline (plist-get raw :headline))
                   (equal (f-filename dir) id)
                   (stringp (org-glance-version:created version))
                   (stringp (org-glance-version:producer version))
                   (org-glance-version--strings-p parents)
                   (org-glance-version--distinct-p parents)
                   (not (member id parents))
                   (pcase kind
                     ('snapshot (stringp digest))
                     ('tombstone (null digest))
                     ('rejection
                      (and (= schema 2) (null digest)
                           (org-glance-version--strings-p targets)
                           (org-glance-version--strings-p observed)
                           (org-glance-version--distinct-p targets)
                           (org-glance-version--distinct-p observed)
                           (cl-every (lambda (target) (member target observed)) targets)
                           (cl-some (lambda (leaf) (not (member leaf targets)))
                                    observed)))))
          (setf (org-glance-version:valid version)
                (org-glance-version--valid-content-p version))
          version))
    (error nil)))

(cl-defun org-glance-version--write
    (headline-dir headline parents producer kind contents _depth)
  "Create one immutable KIND version, optionally holding CONTENTS."
  (let* ((id (org-glance-version--uuid-v7))
         (final (org-glance-version:path headline-dir id))
         (created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (version (make-org-glance-version
                   :schema 1 :headline headline :id id :kind kind
                   :parents (sort (copy-sequence parents) #'string<)
                   :content-sha256 (and contents (secure-hash 'sha256 contents))
                   :created created :producer producer :directory final :valid t)))
    (org-glance-version--publish version contents)))

(cl-defun org-glance-version--publish (version contents)
  "Publish VERSION atomically, optionally storing CONTENTS."
  (let* ((final (org-glance-version:directory version))
         (parent (f-parent final)))
    (f-mkdir-full-path parent)
    (let ((temp (make-temp-file (f-join parent ".version-") t)))
      (unwind-protect
          (progn
            (when contents
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region contents nil (f-join temp "data.org") nil 'silent)))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (concat (json-serialize (org-glance-version--plist version)) "\n")
                            nil (f-join temp "meta.json") nil 'silent))
            (rename-file temp final)
            (setq temp nil))
        (when (and temp (f-exists? temp)) (delete-directory temp t))))
    version))

(cl-defun org-glance-version:write-snapshot
    (headline-dir headline parents producer contents &optional (depth 10))
  "Create a snapshot version below HEADLINE-DIR."
  (org-glance-version--write
   headline-dir headline parents producer 'snapshot contents depth))

(cl-defun org-glance-version:write-tombstone
    (headline-dir headline parents producer &optional (depth 10))
  "Create a tombstone version below HEADLINE-DIR."
  (org-glance-version--write
   headline-dir headline parents producer 'tombstone nil depth))

(cl-defun org-glance-version:read (headline-dir headline)
  "Read HEADLINE's valid version metadata below HEADLINE-DIR."
  (cl-remove-if-not
   (lambda (version)
     (and (org-glance-version--version-event-p version)
          (org-glance-version:valid version)))
   (org-glance-version:history headline-dir headline)))

(cl-defun org-glance-version:history (headline-dir headline)
  "Read HEADLINE's identifiable version history below HEADLINE-DIR.
Each returned node carries payload integrity in its `valid' slot."
  (let* ((root (f-join headline-dir "versions"))
         (candidates
          (when (f-directory? root)
            (cl-loop for dir in (sort (f-directories root) #'string<)
               for meta = (f-join dir "meta.json")
               when (f-exists? meta)
               for raw = (condition-case nil
                             (json-parse-string (f-read-text meta 'utf-8)
                                                :object-type 'plist :array-type 'list
                                                :null-object nil :false-object nil)
                           (error nil))
               for candidate = (and raw
                                    (org-glance-version--candidate dir headline raw))
               when candidate collect candidate)))
         (versions (cl-remove-if-not #'org-glance-version--version-event-p
                                     candidates))
         (known (mapcar #'org-glance-version:id versions))
         (changed t)
         stored
         (legacy (org-glance-version--legacy headline-dir headline)))
    (while changed
      (let ((closed
             (cl-remove-if
              (lambda (version)
                (and (= 2 (org-glance-version:schema version))
                     (cl-some (lambda (parent) (not (member parent known)))
                              (org-glance-version:parents version))))
              versions)))
        (setq changed (/= (length closed) (length versions))
              versions closed
              known (mapcar #'org-glance-version:id versions))))
    (setq stored
          (cl-remove-if
           (lambda (event)
             (and (eq 'rejection (org-glance-version:kind event))
                  (cl-some (lambda (ref) (not (member ref known)))
                           (append (org-glance-version:targets event)
                                   (org-glance-version:observed-leaves event)))))
           candidates))
    (setq stored
          (cl-remove-if
           (lambda (event)
             (and (org-glance-version--version-event-p event)
                  (not (member (org-glance-version:id event) known))))
           stored))
    (if (and legacy
             (not (cl-find (org-glance-version:id legacy) stored
                           :key #'org-glance-version:id :test #'equal)))
        (cons legacy stored)
      stored)))

(cl-defun org-glance-version--legacy (headline-dir headline)
  "Return HEADLINE's implicit legacy root below HEADLINE-DIR."
  (let ((data (f-join headline-dir "data.org")))
    (when (f-file? data)
      (condition-case nil
          (let* ((contents (f-read-text data 'utf-8))
                 (digest (secure-hash 'sha256 contents)))
            (make-org-glance-version
             :schema 1 :headline headline
             :id (org-glance-version:legacy-id headline digest)
             :kind 'snapshot :parents nil :content-sha256 digest
             :created "1970-01-01T00:00:00Z" :producer "legacy"
             :directory headline-dir :valid t))
        (error nil)))))

(cl-defun org-glance-version:migrate-legacy (headline-dir headline)
  "Publish HEADLINE's deterministic root, then remove legacy data.org."
  (when-let* ((legacy (org-glance-version--legacy headline-dir headline))
              (data (f-join headline-dir "data.org"))
              (contents (f-read-text data 'utf-8))
              (id (org-glance-version:id legacy))
              (final (org-glance-version:path headline-dir id)))
    (let ((version (make-org-glance-version
                    :schema 1 :headline headline :id id :kind 'snapshot
                    :parents nil
                    :content-sha256 (org-glance-version:content-sha256 legacy)
                    :created "1970-01-01T00:00:00Z" :producer "migration"
                    :directory final :valid t)))
      (if (f-directory? final)
          (unless (cl-find-if
                   (lambda (held)
                     (and (equal final (org-glance-version:directory held))
                          (equal (org-glance-version:content-sha256 held)
                                 (org-glance-version:content-sha256 version))))
                   (org-glance-version:read headline-dir headline))
            (error "Invalid migration root already exists: %s" final))
        (org-glance-version--publish version contents))
      (delete-file data)
      version)))

(cl-defun org-glance-version:leaves (versions)
  "Return VERSIONS not named as another version's parent."
  (let* ((versions (cl-remove-if-not #'org-glance-version--version-event-p
                                     versions))
         (parents (cl-loop for version in versions
                          append (org-glance-version:parents version))))
    (cl-remove-if (lambda (version)
                    (member (org-glance-version:id version) parents))
                  versions)))

(cl-defun org-glance-version:current (headline-dir headline)
  "Return HEADLINE's unrejected structural leaves, including damaged payloads."
  (org-glance-version:candidates
   (org-glance-version:history headline-dir headline)))

(cl-defun org-glance-version:candidates (history)
  "Return HISTORY's structural Leaves after admitted rejections."
  (let ((rejected (cl-loop for event in history
                           when (eq 'rejection (org-glance-version:kind event))
                           append (org-glance-version:targets event))))
    (cl-remove-if (lambda (version)
                    (member (org-glance-version:id version) rejected))
                  (org-glance-version:leaves history))))

(cl-defun org-glance-version:prune (headline-dir headline depth)
  "Retain DEPTH generations of HEADLINE history and return removed ids.
Zero keeps all history.  A family with several structural leaves is unchanged."
  (when (> depth 0)
    (let* ((history (org-glance-version:history headline-dir headline))
           (leaves (org-glance-version:leaves history)))
      (when (= 1 (length leaves))
        (let ((by-id (make-hash-table :test #'equal))
              (kept (make-hash-table :test #'equal))
              (frontier (list (org-glance-version:id (car leaves))))
              (remaining (1- depth))
              removed)
          (dolist (version history)
            (puthash (org-glance-version:id version) version by-id))
          (puthash (car frontier) t kept)
          (while (and (> remaining 0) frontier)
            (setq frontier
                  (delete-dups
                   (cl-loop for id in frontier
                            for version = (gethash id by-id)
                            when version
                            append (cl-remove-if-not
                                    (lambda (parent) (gethash parent by-id))
                                    (org-glance-version:parents version)))))
            (dolist (id frontier) (puthash id t kept))
            (setq remaining (1- remaining)))
          (dolist (version history (nreverse removed))
            (let ((id (org-glance-version:id version)))
              (when (and (not (gethash id kept))
                         (equal (org-glance-version:directory version)
                                (org-glance-version:path headline-dir id)))
                (delete-directory (org-glance-version:directory version) t)
                (push id removed)))))))))

(cl-defun org-glance-version:snapshot-leaves (headline-dir headline)
  "Return HEADLINE's current snapshot versions below HEADLINE-DIR."
  (cl-remove-if-not
   (lambda (version) (eq 'snapshot (org-glance-version:kind version)))
   (org-glance-version:current headline-dir headline)))

(cl-defun org-glance-version:file-p (path)
  "Return non-nil when PATH is an immutable version data or metadata file."
  (string-match-p
   "/versions/[^/]+/\\(?:data\\.org\\|meta\\.json\\)\\'"
   (expand-file-name path)))

(provide 'org-glance-version)
;;; org-glance-version.el ends here
