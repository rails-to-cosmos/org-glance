(require 'org-glance-module)

(org-glance:require
  eieio
  org
  org-element
  lib.core.exceptions
  lib.utils.helpers
  lib.utils.org)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

;; Headline could appear in 3 states:
;; - Dummy
;; - Raw
;; - Full

(defclass org-glance-headline-location () ; Where things can be stored? File/Buffer/?
  ((position :initarg :position
             :initform (point)
             :type number
             :custom number
             :documentation "Headline position in buffer."))
  "Base class for headline location."
  :abstract t)

(defclass org-glance-headline-file (org-glance-headline-location)
  ((filename :initarg :filename
             :initform (buffer-file-name)
             :type (satisfies f-exists-p)
             :documentation "Name of the existing file."))
  "File location.")

(defclass org-glance-headline-buffer (org-glance-headline-location)
  ((buffer :initarg :buffer-name
           :initform (current-buffer)
           :type (satisfies buffer-live-p)
           :documentation "Name of the existing buffer."))
  "Buffer location.")

(cl-deftype org-element-headline ()
    '(satisfies (lambda (el) (eql 'headline (org-element-type el)))))

(defclass org-glance-headline ()
  ((title :initarg :title
          :type string
          :documentation "The title of headline.")
   (location :initarg :location
             :type org-glance-headline-location
             :documentation "The location of headline.")
   (contents :initarg :contents
             :type string)
   (element :initarg :element
            :type org-element-headline))
  "A base class for tracking headlines.")

;; (cl-defmethod org-glance-thing-serialize ((thing org-glance-thing))
;;   (message "Hello thing: %s"  (oref thing name)))

;; (let ((thing (org-glance-headline :title "Test thing")))
;;   (oref thing :properties))

(defvar org-glance-headline:serde-alist
  `((:raw-value  . (:reader org-glance-headline:title :writer org-glance-headline:title))
    (:begin      . (:reader org-glance-headline:begin :writer org-glance-headline:begin))
    (:file       . (:reader org-glance-headline:file :writer org-glance-headline:file))
    (:commentedp . (:reader org-glance-headline:commented-p :writer org-glance-headline:commented-p))
    (:archivedp  . (:reader org-glance-headline:archived-p :writer org-glance-headline:archived-p))
    (:contains-link-p . (:reader org-glance-headline:contains-link-p :writer (lambda (hl)
                                                                               (save-excursion
                                                                                 (save-restriction
                                                                                   (org-narrow-to-subtree)
                                                                                   (org-end-of-meta-data t)
                                                                                   (when (re-search-forward org-any-link-re nil t)
                                                                                     'contains-link))))))
    (:contains-property-p . (:reader org-glance-headline:contains-property-p :writer (lambda (hl)
                                                                                       (save-excursion
                                                                                         (save-restriction
                                                                                           (org-narrow-to-subtree)
                                                                                           (org-end-of-meta-data t)
                                                                                           (when (re-search-forward org-glance:key-value-pair-re nil t)
                                                                                             'contains-properties))))))
    (:encryptedp . (:reader org-glance-headline:encrypted? :writer (lambda (hl)
                                                                     (save-excursion
                                                                       (org-end-of-meta-data t)
                                                                       (when (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")
                                                                         'encrypted)))))
    (:buffer . (:reader org-glance-headline:buffer :writer (lambda (hl)
                                                             (condition-case nil
                                                                 (buffer-name (get-file-buffer (org-glance-headline:file hl)))
                                                               (wrong-type-argument (buffer-name)))))))
  "Map `org-element-property' to `org-glance' extractor method.

It is safe (in terms of backward/forward compability of
metastores) to append properties to this map.

Do not modify existing properties without backfilling of
metastore.")

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (cl-loop
     for (property . methods) in org-glance-headline:serde-alist
     when (plist-get methods :reader)
     collect (funcall (plist-get methods :reader) headline)))

(cl-defun org-glance-headline:deserialize (dump)
  "Deserialize DUMP to minimal headline."
  (cl-loop
     with element = (org-element-create 'headline)
     for (property . _) in org-glance-headline:serde-alist
     for index from 0
     do (org-glance-headline:enrich element property (nth index dump))
     finally (return element)))

(cl-defun org-glance-headline:enrich (element &rest kwargs)
  "Enrich `org-element' ELEMENT with KWARGS properties."
  (declare (indent 1))
  (cl-loop
     for (key value) on kwargs by #'cddr
     do (org-element-put-property element key value)
     finally (return element)))

(cl-defun org-glance-headline:create-from-element-at-point ()
  "Create `org-glance-headline' from element at point."
  (let ((prototype (org-element-at-point)))
    (when (eql 'headline (org-element-type prototype))
      (cl-loop
         for (property . methods) in org-glance-headline:serde-alist
         for index from 0
         do (org-glance-headline:enrich prototype property (funcall (plist-get methods :writer) prototype))
         finally (return prototype)))))

(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance:ensure-at-heading)
  (beginning-of-line)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (org-glance-headline:create-from-element-at-point))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (org-glance-headline:search-parents)))

(cl-defun org-glance-headline:active? (&optional (headline (org-element-at-point)))
  (and (org-glance-headline-p headline)
       (not (org-glance-headline:commented-p headline))
       (not (org-glance-headline:archived-p headline))))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (while (and (outline-previous-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-forward ()
  (interactive)
  (while (and (outline-next-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :commentedp headline)
    t))

(cl-defun org-glance-headline:archived-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :archivedp headline)
    t))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE, cleanup links."
  (with-temp-buffer
    (insert (or (org-element-property :TITLE headline)
                (org-element-property :raw-value headline)
                ""))
    (cl-loop
       for (title beg end) in (org-element-map (org-element-parse-buffer) 'link
                                (lambda (link) (list
                                                (substring-no-properties
                                                 (or (nth 2 link)
                                                     (org-element-property :raw-link link)))
                                                (org-element-property :begin link)
                                                (org-element-property :end link))))
       collect title into titles
       collect (s-trim (buffer-substring-no-properties beg end)) into links
       finally (return (cl-loop
                          initially (goto-char (point-min))
                          for i upto (1- (length titles))
                          do (replace-string (nth i links) (nth i titles))
                          finally (return (buffer-substring-no-properties (point-min) (point-max))))))))

(cl-defun org-glance-headline:priority (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-some-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:level (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (let ((buffer (org-element-property :buffer headline)))
    (cond ((bufferp buffer) (buffer-name buffer))
          (t buffer))))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:tags (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (org-glance:log-debug "Searching buffer %s by id %s" (current-buffer) id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (string= (org-glance-headline:id elem) id)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:HEADLINE-NOT-FOUND "Headline not found in file %s: %s" (buffer-file-name) id))
    (when (> (length points) 1)
      (org-glance:log-warning "Headline ID %s is not unique in file %s" id (buffer-file-name)))
    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE."
  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline))
         (revert-without-query (list file)))

    (cond ((and file (file-exists-p file)) (find-file file))
          ((and buffer (buffer-live-p buffer)) (switch-to-buffer buffer))
          (t (org-glance:log-warning "File and buffer not found for visiting. Using current buffer...")))

    (org-glance:log-debug "We are now visiting headline buffer %s, let's remove restrictions" (current-buffer))
    (widen)
    (org-glance:log-debug "Search headline in buffer")
    (cond (id (org-glance-headline:search-buffer-by-id id))
          (t (goto-char (org-glance-headline:begin headline))))))

(cl-defmacro org-glance-headline:narrow (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (save-excursion
       (save-restriction
         (org-glance-headline:visit ,headline)
         (org-narrow-to-subtree)
         ,@forms))))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (&optional (headline (org-glance-headline:at-point)))
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (org-glance:log-debug "File DEBUG: %s" file)
    (org-glance:log-debug "Buffer DEBUG: %s" buffer)
    (cond (file (with-temp-buffer
                  (org-glance:log-debug "Extract contents for headline %s from file %s"
                                        (org-glance-headline:id headline)
                                        file)
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (org-glance:log-debug "I'm in buffer %s" buffer)
                    (org-glance:log-debug "Extract contents for headline %s from buffer %s" (org-glance-headline:id headline) file)
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                          (org-narrow-to-subtree)
                          (org-glance-headline:promote-to-the-first-level)
                          (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))
          (t (org-glance-exception:HEADLINE-NOT-FOUND "Unable to determine headline location.")))))

(cl-defgeneric org-glance-headline:extract-from (scope)
  "Extract `org-glance-headlines' from scope.")

(cl-defmethod org-glance-headline:extract-from ((f string))
  "Extract headlines from file F."
  (if-let (b (get-buffer f)) ;; buffer name
      (org-glance-headline:extract-from b)
    (with-temp-buffer
      (org-glance:log-debug "Scan file %s" f)
      (insert-file-contents f)
      (org-mode)
      (cl-loop
         for headline in (org-glance-headline:extract-from (current-buffer))
         collect (org-glance-headline:enrich headline :file (abbreviate-file-name f))))))

(cl-defmethod org-glance-headline:extract-from ((b buffer))
  "Extract headlines from buffer B."
  (with-current-buffer b
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (e)
        (when (org-glance-headline-p e)
          (save-excursion
            (goto-char (org-glance-headline:begin e))
            (org-glance-headline:enrich (org-glance-headline:create-from-element-at-point)
              :buffer b)))))))

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:with-materialized-headline headline
    (goto-char (org-log-beginning t))
    (insert note "\n")))

(cl-defmacro org-glance-headline:format (headline)
  (declare (indent 1) (debug t))
  `(let* ((id (org-glance-headline:id ,headline))
          (state (org-glance-headline:state ,headline))
          (state-label (if (string-empty-p state) "" (format "*%s*" state)))
          (title (org-glance-headline:title ,headline))
          (stateless-title (replace-regexp-in-string (format "^%s[[:space:]]*" state) "" title))
          (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
     (s-trim
      (org-glance:format "${state-label} [[org-glance-visit:${id}][${stateless-title}]]"))))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:encrypt-region beg end password)))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
        (end (save-excursion (org-end-of-subtree t) (point))))
    (org-glance:decrypt-region beg end password)))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

;; Relations

(defconst org-glance-relation:forward "Related to")
(defconst org-glance-relation:backward "Referred from")
(defconst org-glance-relation-re
  (rx (seq bol
           (0+ (any "\t -"))
           (or (literal org-glance-relation:forward)
               (literal org-glance-relation:backward))
           (0+ (any "\t *"))
           (group-n 1 (0+ word)) ;; state
           (0+ (any "\t *"))
           "="
           (group-n 2 (1+ (any word))) ;; category
           "="
           (0+ (any "\t "))
           "[[org-glance-visit:"
           (group-n 3 (1+ (not "]"))) ;; id
           "]["
           (group-n 4 (1+ (not "]"))) ;; title
           "]]"
           (0+ (any "\t "))
           "on"
           (0+ (any "\t "))
           (regexp org-ts-regexp-inactive)
           (0+ (any "\t "))
           eol))
  "Matches relation.")

(cl-defun org-glance-headline:next-relation ()
  (condition-case nil
      (re-search-forward org-glance-relation-re)
    (error nil)))

(cl-defun org-glance-headline:relation-category ()
  (substring-no-properties (match-string 2)))

(cl-defun org-glance-headline:relation-id ()
  (substring-no-properties (match-string 3)))

(cl-defun org-glance-headline:relations (&optional (headline (org-glance-headline:at-point)))
  "Get all first-level relations of HEADLINE."
  (org-glance-headline:with-materialized-headline headline
    (cl-loop
       while (org-glance-headline:next-relation)
       for id = (org-glance-headline:relation-id)
       for relation = (org-glance-metastore:get-headline id)
       when relation
       collect relation)))

(cl-defun org-glance-headline:relations* (&optional (headline (org-glance-headline:at-point)))
  "Get all relations of HEADLINE recursively."
  (let* ((visited (make-hash-table :test 'equal))
         (headlines (list headline)))
    (cl-loop
       while headlines
       for headline = (pop headlines)
       for relations = (org-glance-headline:relations headline)
       for id = (org-glance-headline:id headline)
       for title = (org-glance-headline:title headline)
       for relation-titles = (mapcar #'org-glance-headline:title relations)
       unless (gethash id visited nil)
       collect (cons title relation-titles)
       into result
       do
         (puthash id t visited)
         (cl-loop
            for relation in relations
            for relation-id = (org-glance-headline:id relation)
            unless (gethash relation-id visited nil)
            do
              (push relation headlines)
              (pp (mapcar #'org-glance-headline:title headlines)))
       finally (return result))))

(cl-defun org-glance-headline:hash (&optional (headline (org-glance-headline:at-point)))
  (let ((contents (org-glance-headline:contents headline)))
    (with-temp-buffer
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:add-relation (source target &key (rel org-glance-relation:forward))
  (interactive)
  (let* ((target-title (org-glance-headline:format target))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
    (org-glance-headline:add-log-note (org-glance:format "- ${rel} ${target-title} on ${now}") source)))

;; (cl-defun org-glance-headline:add-biconnected-relation
;;     (source target
;;      &key
;;        (source->target org-glance-relation:forward)
;;        (target->source org-glance-relation:backward))
;;   (when source
;;     (save-excursion
;;       (save-restriction
;;         (org-save-outline-visibility t
;;           (org-glance-headline:add-relation source target :rel source->target)
;;           (unless (eql source target)
;;             (org-glance-headline:add-relation target source :rel target->source)))))))

(cl-defun org-glance-headline:contains-property-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-property-p headline)
    'contains-property))

(cl-defun org-glance-headline:contains-link-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-link-p headline)
    'contains-link))

(cl-defun org-glance-headline:encrypted? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :encryptedp headline)
    'encrypted))

(cl-defun org-glance-headline:string-to-class (tag)
  (intern (s-downcase tag)))

(cl-defun org-glance-headline:classes (&optional (headline (org-glance-headline:at-point)))
  (cl-loop
     for tag in (org-element-property :tags headline)
     collect (org-glance-headline:string-to-class tag)))

(cl-defun org-glance-headline:generate-id (class)
  (format "%s-%s-%s"
          class
          (s-join "-" (mapcar #'number-to-string (current-time)))
          (secure-hash 'md5 (buffer-string))))

(cl-defun org-glance-headline:scheduled (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :deadline headline))

(cl-defun org-glance-headline:repeated-p (&optional (headline (org-glance-headline:at-point)))
  (when-let (repeater (or (when-let (scheduled (org-glance-headline:scheduled headline))
                            (plist-get (cadr scheduled) :repeater-value))
                          (when-let (deadline (org-glance-headline:deadline headline))
                            (plist-get (cadr deadline) :repeater-value))
                          (when (and (boundp 'org-advanced-schedule-mode)
                                     org-advanced-schedule-mode
                                     (org-element-property :ADVANCED_SCHEDULE headline))
                            1)))
    (> repeater 0)))

(cl-defun org-glance-headline:generate-directory (location title)
  (abbreviate-file-name
   (make-temp-file
    (-org-glance:make-file-directory
     (f-join location
             (concat (format-time-string "%Y-%m-%d_")
                     (->> title
                          (replace-regexp-in-string "[^a-z0-9A-Z_]" "-")
                          (replace-regexp-in-string "\\-+" "-")
                          (replace-regexp-in-string "\\-+$" "")
                          (s-truncate 30))
                     "-")))
    'directory)))

(org-glance:provide)
