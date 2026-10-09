;; -*- lexical-binding: t -*-

;;; org-glance-version.el --- Immutable headline version DAG

(require 'cl-lib)
(require 'f)
(require 'json)
(require 'subr-x)

(cl-defstruct (org-glance-version (:predicate org-glance-version?)
                                  (:conc-name org-glance-version:))
  schema headline id kind parents content-sha256 created producer directory valid)

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

(cl-defun org-glance-version--valid-content-p (dir raw)
  "Return non-nil when DIR's content agrees with metadata RAW."
  (condition-case nil
      (let ((data (f-join dir "data.org"))
            (kind (plist-get raw :kind))
            (digest (plist-get raw :contentSha256)))
        (cond
         ((equal kind "tombstone") (and (null digest) (not (f-exists? data))))
         ((equal kind "snapshot")
          (and digest (f-file? data)
               (equal digest (secure-hash 'sha256 (f-read-text data 'utf-8)))))
         (t nil)))
    (error nil)))

(cl-defun org-glance-version--write (headline-dir headline parents producer kind contents)
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
    (headline-dir headline parents producer contents)
  "Create a snapshot version below HEADLINE-DIR."
  (org-glance-version--write headline-dir headline parents producer 'snapshot contents))

(cl-defun org-glance-version:write-tombstone (headline-dir headline parents producer)
  "Create a tombstone version below HEADLINE-DIR."
  (org-glance-version--write headline-dir headline parents producer 'tombstone nil))

(cl-defun org-glance-version:read (headline-dir headline)
  "Read HEADLINE's valid version metadata below HEADLINE-DIR."
  (cl-remove-if-not #'org-glance-version:valid
                    (org-glance-version:history headline-dir headline)))

(cl-defun org-glance-version:history (headline-dir headline)
  "Read HEADLINE's identifiable version history below HEADLINE-DIR.
Each returned node carries payload integrity in its `valid' slot."
  (let* ((root (f-join headline-dir "versions"))
         (stored
          (when (f-directory? root)
            (cl-loop for dir in (sort (f-directories root) #'string<)
               for meta = (f-join dir "meta.json")
               when (f-exists? meta)
               for raw = (condition-case nil
                             (json-parse-string (f-read-text meta 'utf-8)
                                                :object-type 'plist :array-type 'list
                                                :null-object nil :false-object nil)
                           (error nil))
               when (and raw (= 1 (plist-get raw :version))
                         (equal headline (plist-get raw :headline))
                         (equal (f-filename dir) (plist-get raw :id)))
               collect (make-org-glance-version
                        :schema 1 :headline headline :id (plist-get raw :id)
                        :kind (intern (plist-get raw :kind))
                        :parents (plist-get raw :parents)
                        :content-sha256 (plist-get raw :contentSha256)
                        :created (plist-get raw :created)
                        :producer (plist-get raw :producer) :directory dir
                        :valid (org-glance-version--valid-content-p dir raw)))))
         (legacy (org-glance-version--legacy headline-dir headline)))
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
  (let ((parents (cl-loop for version in versions
                          append (org-glance-version:parents version))))
    (cl-remove-if (lambda (version)
                    (member (org-glance-version:id version) parents))
                  versions)))

(cl-defun org-glance-version:current (headline-dir headline)
  "Return HEADLINE's structural leaves, including damaged payloads."
  (org-glance-version:leaves
   (org-glance-version:history headline-dir headline)))

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
