(require 'org-glance-module)

(org-glance:require
  org
  org-element
  lib.core.exceptions
  lib.utils.helpers
  lib.utils.org)

(defconst org-glance-headline:key-value-pair-re "^\\([[:word:],[:blank:]]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property.
Return HEADLINE or nil if it is not a proper `org-glance-headline'."
  (when (not (null (org-element-property :ORG_GLANCE_ID headline)))
    headline))

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (list (org-glance-headline:title headline)
        (org-glance-headline:begin headline)
        (org-glance-headline:file headline)))

(cl-defun org-glance-headline:deserialize (dump)
  "Deserialize DUMP to minimal headline."
  (cl-destructuring-bind (title begin file) dump
    (org-element-create 'headline
                        (list :raw-value title
                              :begin begin
                              :file file))))

(cl-defun org-glance-headline:goto-beginning-of-current-headline ()
  "Jump to headline at point and build `org-glance-headline' object from `org-element' at point.
If headline is not an `org-glance-headline', traverse parents."
  (org-glance:ensure-at-heading)
  (beginning-of-line)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (when (org-glance-headline-p)
    (org-glance-headline:enrich (org-element-at-point)
      :file (if-let (file-name (buffer-file-name)) (abbreviate-file-name file-name))
      :buffer (current-buffer))))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (org-glance-headline:goto-beginning-of-current-headline)))

(cl-defun org-glance-headline:active? (&optional (headline (org-element-at-point)))
  (and (org-glance-headline-p headline)
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (while (and (outline-previous-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-forward ()
  (interactive)
  (while (and (outline-next-heading) (not (org-glance-headline:active?))))
  (when (org-glance-headline:active?) (org-glance-headline:at-point)))

(cl-defun org-glance-headline:enrich (element &rest kwargs)
  "Enrich `org-element' ELEMENT with KWARGS properties.
Default enrichment is as follows:
- Add FILE property to `org-element'."
  (declare (indent 1))
  (cl-loop
     for (key value) on kwargs by #'cddr
     do (org-element-put-property element key value)
     finally (return element)))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :commentedp headline))

(cl-defun org-glance-headline:archived? (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :archivedp headline))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:priority (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (-> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :file headline))

(cl-defun org-glance-headline:level (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :buffer headline)
      (get-file-buffer (org-glance-headline:file headline))))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:end (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :end headline))

(cl-defun org-glance-headline:class (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :ORG_GLANCE_CLASS headline))

(cl-defun org-glance-headline:view-ids (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (string= (org-glance-headline:id elem) id)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" (buffer-file-name) id))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s" id (buffer-file-name)))

    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:search-buffer (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  "Visit HEADLINE by id. Grab source file from metastore."
  (let* ((file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline)))

    (cond (file (if (file-exists-p file) (find-file file)
                  (org-glance-exception:metastore-outdated "File not found: %s" file)))
          (buffer (if (bufferp buffer)
                      (switch-to-buffer buffer)
                    (org-glance-exception:metastore-outdated "Buffer not found: %s" (buffer-name buffer)))))

    ;; we are now visiting headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (org-glance-headline:search-buffer headline)

    ;; for interactive usage only
    (org-glance:expand-parents)
    (org-overview)
    (org-cycle 'contents)

    (org-glance-headline:at-point)))

(cl-defmacro org-glance-headline:narrow (headline &rest forms &key (save-excursion t) &allow-other-keys)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let* ((file (org-glance-headline:file ,headline))
          (file-buffer (condition-case nil
                           (get-file-buffer file)
                         (wrong-type-argument (org-glance-headline:buffer ,headline))))
          (visited-buffer (current-buffer))
          result)

     (if ,save-excursion ;; fixme
         (save-window-excursion
           (org-glance-headline:visit ,headline)

           (save-restriction
             (org-narrow-to-subtree)
             (when (= (point-max) (save-excursion
                                    (org-end-of-meta-data)
                                    (point)))
               (goto-char (point-max))
               (insert "\n"))
             (setq result (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                            ,@forms)))

           (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
                 ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer) (bury-buffer file-buffer)))
                 (file (save-buffer) (kill-buffer (get-file-buffer file)))))
       (progn
         (org-glance-headline:visit ,headline)

         (save-restriction
           (org-narrow-to-subtree)
           (when (= (point-max) (save-excursion
                                  (org-end-of-meta-data)
                                  (point)))
             (goto-char (point-max))
             (insert "\n"))
           (setq result (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                          ,@forms)))

         (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
               ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                           (bury-buffer file-buffer)))
               (file (save-buffer) (kill-buffer (get-file-buffer file))))))

     result))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (&optional (headline (org-glance-headline:at-point)))
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer headline)
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer headline)
                          (org-narrow-to-subtree)
                          (org-glance-headline:promote-to-the-first-level)
                          (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))
          (t (org-glance-exception:headline-not-found "Unable to determine headline location.")))))

(cl-defun org-glance-headline:links (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (org-end-of-meta-data t)
    (narrow-to-region (point) (point-max))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(cl-defun org-glance-headline:scan-file (&optional (file (buffer-file-name)))
  (with-temp-buffer
    (insert-file-contents file)
    (hack-local-variables)
    (unless (alist-get 'org-glance-overview-mode (buffer-local-variables))
      (org-mode)
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (el)
          (-some-> el
            (org-glance-headline-p)
            (org-glance-headline:enrich
                :file (abbreviate-file-name file)
                :buffer (get-file-buffer file))))))))

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (goto-char (org-log-beginning t))
    (insert note "\n")
    (when (buffer-file-name)
      (save-buffer))))

(cl-defun org-glance-headline:rename (headline title)
  (org-glance-headline:narrow headline
    (let ((old-title (org-glance-headline:raw-value headline))
          (new-title (s-replace-regexp "[[:space:]]" " " title)))
      (org-glance-headline:add-log-note
       (org-glance:format "- Renamed from \"${old-title}\" to \"${new-title}\" on ${now}"))
      (org-glance-headline:goto-beginning-of-current-headline)
      (org-beginning-of-line)
      (org-kill-line)
      (insert new-title)
      (org-align-tags))))

(cl-defmacro org-glance-headline:format (headline &key (format "${label}=${classes}= [[org-glance-visit:${id}][${stateful-title}]]"))
  (declare (indent 1) (debug t))
  `(let* ((id (org-glance-headline:id ,headline))
          (state (org-glance-headline:state ,headline))
          (label (if (string-empty-p state) " " (format " *%s* " state)))
          (original-title (org-glance-headline:title ,headline))
          (stateful-title (s-replace-regexp (format "^%s[[:space:]]*" state) "" original-title))
          (classes (s-join ", " (org-glance-headline:view-ids ,headline)))
          (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
     (s-trim (org-glance:format ,format))))

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
  (org-glance-headline:narrow headline
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

(cl-defun org-glance-headline:add-relation
    (source target &key (rel org-glance-relation:forward))
  (interactive)
  (let* ((target-title (org-glance-headline:format target))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time)))
         (log-entry (org-glance:format "- ${rel} ${target-title} on ${now}")))
    (org-glance-headline:add-log-note log-entry source)))

(cl-defun org-glance-headline:add-biconnected-relation
    (source target
     &key
       (source->target org-glance-relation:forward)
       (target->source org-glance-relation:backward))
  (when source
    (save-excursion
      (save-restriction
        (org-save-outline-visibility t
          (org-glance-headline:add-relation source target :rel source->target)
          (unless (eql source target)
            (org-glance-headline:add-relation target source :rel target->source)))))))

(cl-defun org-glance-headline:materialized-buffer-name (&optional (headline (org-glance-headline:at-point)))
  (concat "org-glance:<" (org-glance-headline:title headline) ">"))

(cl-defun org-glance-headline:encrypted? (&optional (headline (org-glance-headline:at-point)))
  (s-contains? "aes-encrypted" (org-glance-headline:contents headline)))

(cl-defun org-glance-headline:key-value-storage? (&optional (headline (org-glance-headline:at-point)))
  (save-excursion
    (org-glance-headline:narrow headline
      (org-end-of-meta-data)
      (search-forward-regexp org-glance-headline:key-value-pair-re nil t))))

(cl-defun org-glance-headline:key-value-pairs (&optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (cl-loop
       while (condition-case nil
                 (re-search-forward org-glance-headline:key-value-pair-re)
               (search-failed nil))
       collect (s-trim (substring-no-properties (match-string 1))) into keys
       collect (s-trim (substring-no-properties (match-string 2))) into vals
       finally (return (-zip keys vals)))))

(org-glance:provide)
