(require 'org-glance-module)

(org-glance:require
  org
  org-element
  src.core.exceptions
  src.utils.helpers)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID headline)
    headline))

(defvar org-glance-headline:serde-alist nil
  "Map `org-element-property' to `org-glance' extractor method.

It is safe (in terms of backward/forward compability of
metastores) to append properties to this map.

Do not modify existing properties without backfilling of
metastore.")

(setq org-glance-headline:serde-alist
      `((:raw-value  . (:reader org-glance-headline:title      :writer org-glance-headline:title))
        (:begin      . (:reader org-glance-headline:begin      :writer org-glance-headline:begin))
        (:file       . (:reader org-glance-headline:file       :writer org-glance-headline:file))
        (:commentedp . (:reader org-glance-headline:commented? :writer org-glance-headline:commented?))
        (:archivedp  . (:reader org-glance-headline:archived?  :writer org-glance-headline:archived?))
        (:contains-link-p    . (:reader org-glance-headline:contains-link?    :writer (lambda (hl)
                                                                                        (save-excursion
                                                                                          (save-restriction
                                                                                            (org-narrow-to-subtree)
                                                                                            ;; (org-end-of-meta-data t)
                                                                                            (when (org-glance-parse-links)
                                                                                              'contains-link))))))
        (:contains-property-p        . (:reader org-glance-headline:contains-property?       :writer (lambda (hl)
                                                                                                       (save-excursion
                                                                                                         (save-restriction
                                                                                                           (org-narrow-to-subtree)
                                                                                                           (org-end-of-meta-data t)
                                                                                                           (when (re-search-forward org-glance:key-value-pair-re nil t)
                                                                                                             'contains-properties))))))
        (:encryptedp . (:reader org-glance-headline:encrypted-p :writer (lambda (hl)
                                                                          (save-excursion
                                                                            (org-end-of-meta-data t)
                                                                            (when (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")
                                                                              'encrypted)))))
        (:buffer . (:reader org-glance-headline:buffer :writer (lambda (hl)
                                                                 (condition-case nil
                                                                     (buffer-name (get-file-buffer (org-glance-headline:file hl)))
                                                                   (wrong-type-argument (buffer-name))))))
        (:closed . (:reader org-glance-headline:closed? :writer org-glance-headline:closed?))))

(cl-defun org-glance-headline:serialize (headline)
  "Serialize HEADLINE to store it on disk."
  (cl-loop
     for (property . methods) in org-glance-headline:serde-alist
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
  (org-glance-ensure-at-heading)
  (while (and (not (org-glance-headline-p))
              (not (org-before-first-heading-p))
              (not (bobp)))
    (org-up-heading-or-point-min))
  (org-glance-headline:create-from-element-at-point))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (when (condition-case nil
              (or (org-at-heading-p) (org-back-to-heading))
            (error nil))
      (org-glance-headline:search-parents))))

(defun org-glance-headline:is-active-todo (state)
  "Check if the given STATE represents an active TODO item."
  (not (member state org-done-keywords-for-agenda)))

(cl-defun org-glance-headline:active? (&optional (headline (org-element-at-point)))
  (and (org-glance-headline-p headline)
       (org-glance-headline:is-active-todo (org-glance-headline:state headline))
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))
       (not (org-glance-headline:closed? headline))))

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (when-let (headline (org-glance-headline:at-point))
    (goto-char (org-glance-headline:begin headline))
    (forward-char -1))

  (while (and (not (org-before-first-heading-p))
              (not (bobp))
              (not (org-glance-headline:at-point)))
    (outline-previous-heading))

  (if-let (headline (org-glance-headline:at-point))
      (progn (goto-char (org-glance-headline:begin headline))
             headline)
    (progn (goto-char (point-min)))))

(cl-defun org-glance-headline:search-forward ()
  (interactive)

  (let ((headline (org-glance-headline:at-point))
        next-headline)

    (save-excursion

      (when headline
        (if (org-glance-headline:end headline)
            (goto-char (org-glance-headline:end headline))
          (goto-char (point-max)))

        (condition-case nil
            (progn (beginning-of-line)
                   (forward-line 1))
          (end-of-buffer nil)))

      (setq next-headline (org-glance-headline:at-point))
      (while (and (not (eobp))
                  (or (not next-headline)
                      (equal headline next-headline)))
        (forward-line 1)
        (setq next-headline (org-glance-headline:at-point))))

    (when (and next-headline (not (equal headline next-headline)))
      (goto-char (org-glance-headline:begin next-headline))
      next-headline)))

(cl-defun org-glance-headline:list ()
  (save-excursion
    (goto-char (point-min))

    (let (result)
      (when-let (headline (org-glance-headline:at-point))
        (push headline result))

      (while-let ((headline (org-glance-headline:search-forward)))
        (push headline result))

      result)))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :commentedp headline)
    t))

(cl-defun org-glance-headline:archived? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :archivedp headline)
    t))

(cl-defun org-glance-headline:closed? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :closed headline)
    t))

(cl-defun org-glance-headline:raw-value (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :raw-value headline))

(cl-defun org-glance-parse-links ()
  "Simple org-link parser, return list of cons cells (link . contents)."
  (let ((descriptions (cl-loop for (key . val) in (org-glance-buffer-key-value-pairs)
                         collect (cons val key)))
        (links (cl-loop
                  for element in (org-element-map (org-element-parse-buffer) 'link #'identity)
                  for beg = (org-element-property :begin element)
                  for end = (org-element-property :end element)
                  for title = (substring-no-properties
                               (or (-some->> element
                                     org-element-contents
                                     org-element-interpret-data)
                                   (org-element-property :raw-link element)))
                  for link = (s-trim (buffer-substring-no-properties beg end))
                  collect title into titles
                  collect link into links
                  collect beg into positions
                  finally return (-zip links titles positions))))
    (cl-loop
       for (link title pos) in links
       for description = (alist-get link descriptions nil nil #'string=)
       when description
       collect (list link description pos)
       else
       collect (list link title pos))))

(cl-defun org-glance-remove-links (&rest types)
  (save-excursion
    (cl-loop while (re-search-forward (concat "[[:blank:]]?" org-link-any-re) nil t)
       do (let* ((link (s-split-up-to ":" (substring-no-properties (or (match-string 2) "")) 1))
                 (type (intern (car link)))
                 (id (cadr link)))
            (when (memq type types)
              (delete-region (match-beginning 0) (match-end 0)))))))

(cl-defun org-glance-replace-links-with-titles ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop
       for (link title pos) in (org-glance-parse-links)
       do (save-excursion (replace-string link title)))))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE, cleanup links."
  (with-temp-buffer
    (save-excursion
      (insert (or (org-element-property :TITLE headline)
                  (org-element-property :raw-value headline)
                  "")))
    (org-glance-remove-links 'org-glance-overview 'org-glance-state)
    (org-glance-replace-links-with-titles)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-headline:alias (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE considering alias property."
  (with-temp-buffer
    (save-excursion
      (insert (or
               (org-element-property :ALIAS headline)
               (org-element-property :TITLE headline)
               (org-element-property :raw-value headline)
               "")))
    (org-glance-remove-links 'org-glance-overview 'org-glance-state)
    (org-glance-replace-links-with-titles)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

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

(cl-defun org-glance-headline:end (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :contents-end headline))

(cl-defun org-glance-headline:tags (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (el) (when (string= (org-glance-headline:id el) id)
                                 (org-element-property :begin el))))))
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

    (widen)
    (cond (id (org-glance-headline:search-buffer-by-id id))
          (t (goto-char (org-glance-headline:begin headline))))))

(cl-defmacro org-glance:with-file-visited (file &rest forms)
  "Visit FILE, execute FORMS and close it if it was closed before visit."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (let ((inhibit-startup-hooks t)
           (inhibit-modification-hooks t)
           (buffer-lived-p (buffer-live-p (get-file-buffer ,file)))
           (buffer (find-file-noselect ,file)))
       (unwind-protect
           (with-current-buffer buffer
             ,@forms)
         (unless buffer-lived-p
           (kill-buffer buffer))))))

(cl-defmacro org-glance:with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup))
         (id (org-glance-headline:id ,headline))
         (file (org-glance-headline:file ,headline))
         (buffer (org-glance-headline:buffer ,headline)))
     (cond (file (org-glance:with-file-visited file
                   (org-glance-headline:search-buffer-by-id id)
                   (org-glance:with-headline-at-point ,@forms)))
           ((and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (org-glance-headline:search-buffer-by-id id)
              (org-glance:with-headline-at-point ,@forms)))
           (t (org-glance-exception:HEADLINE-NOT-FOUND (prin1-to-string ,headline))))))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance-ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:replace-headline-at-point (new-contents)
  (let ((beg (org-glance-headline:begin))
        (end (save-excursion (org-end-of-subtree t)))
        (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert new-contents)))

(cl-defun org-glance-headline-contents (&optional (headline (org-glance-headline:at-point)))
  "Extracts HEADLINE contents.
FIXME. Unstable one. Refactor is needed."
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-glance:log-debug "Extract contents for headline %s from file %s" (org-glance-headline:id headline) file)
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (org-glance:log-debug "Extract contents for headline %s from buffer %s" (org-glance-headline:id headline) buffer)
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                          (org-narrow-to-subtree)
                          (let ((contents (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
                            (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (goto-char (point-min))
                              (outline-next-heading)
                              (org-glance-headline:promote-to-the-first-level)
                              (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))))
          (t (org-glance-exception:HEADLINE-NOT-FOUND "Unable to determine headline location")))))

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

(cl-defun org-glance-headline:add-log-note (string &rest objects)
  (org-glance:with-headline-at-point
   (goto-char (org-log-beginning t))
   (insert (apply #'format string objects) "\n")))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance:encrypt-region beg end password))))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance:decrypt-region beg end password))))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

;; Relations

(cl-defun org-glance-headline-relation:subtask-p (relation)
  (memq (org-element-property :type relation) '(subtask subtask-done)))

(cl-defun org-glance-headline:subtasks ()
  (cl-loop
     for relation in (org-glance-headline-relations)
     when (org-glance-headline-relation:subtask-p relation)
     collect relation))

(cl-defun org-glance-headline:hash (&optional (headline (org-glance-headline:at-point)))
  (let ((contents (org-glance-headline-contents headline)))
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:contains-property? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-property-p headline)
    'contains-property))

(cl-defun org-glance-headline:contains-link? (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :contains-link-p headline)
    'contains-link))

(cl-defun org-glance-headline:encrypted-p (&optional (headline (org-glance-headline:at-point)))
  (when (org-element-property :encryptedp headline)
    'encrypted))

(cl-defun org-glance-headline:string-to-class (tag)
  (intern (s-downcase tag)))

(cl-defun org-glance-headline:classes (&optional (headline (org-glance-headline:at-point)))
  (cl-loop
     for tag in (org-element-property :tags headline)
     collect (org-glance-headline:string-to-class tag)))

(cl-defun org-glance-headline:scheduled (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :deadline headline))

(cl-defun org-glance-headline:repeated-p ()
  (org-tss-headline-repeated-p))

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

(cl-defun org-glance-headline:overview ()
  "Return HEADLINE high-level usability characteristics."
  (org-glance:with-headline-at-point
   (cl-flet ((org-list (&rest items) (org-glance-join-but-null "\n- " items))
             (org-newline (&rest items) (org-glance-join-but-null "\n" items)))
     (let ((timestamps (cl-loop for timestamp in (-some->> (org-tss-headline-timestamps)
                                                   (org-tss-filter-active)
                                                   (org-tss-sort-timestamps))
                                collect (org-element-property :raw-value timestamp)))
           (header (save-excursion
                     (goto-char (point-min))
                     (org-end-of-meta-data)
                     (s-trim (buffer-substring-no-properties (point-min) (point)))))
           (clocks (org-glance:with-headline-at-point
                    (cl-loop while (re-search-forward org-clock-line-re (point-max) t)
                             collect (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))
           (relations (org-glance-headline-relations))
           (tags (org-get-tags-string))
           (state (org-glance-headline:state))
           (id (org-glance-headline:id))
           (title (org-glance-headline:title))
           (priority (org-glance-headline:priority))
           (closed (org-element-property :closed (org-element-at-point)))
           (schedule (org-glance-headline:scheduled))
           (deadline (org-glance-headline:deadline))
           (encrypted (org-glance-headline:encrypted-p))
           (repeated (org-glance-headline:repeated-p))
           (linked (org-glance-headline:contains-link?)))
       (with-temp-buffer
         (insert
          (concat
           "* "
           state
           (if (string-empty-p state)
               ""
             " ")
           (if priority
               (concat "[#" (char-to-string priority) "]" " ")
             "")
           title
           " "
           tags
           "\n"
           (if (and closed (listp closed))
               (concat "CLOSED: "
                       (org-element-property :raw-value closed)
                       (if (or schedule deadline)
                           " "
                         ""))
             "")
           (if schedule
               (concat "SCHEDULED: "
                       (org-element-property :raw-value schedule)
                       (if deadline
                           " "
                         ""))
             "")
           (if deadline
               (concat "DEADLINE: " (org-element-property :raw-value deadline))
             "")
           (if (or schedule deadline closed)
               "\n"
             "")

           ":PROPERTIES:\n"
           ":ORG_GLANCE_ID: " id "\n"
           ":DIR: " (abbreviate-file-name default-directory) "\n"
           ":END:"

           (org-glance-join-but-null "\n\n"
             (list

              (when (or encrypted linked)
                (concat "*Features*"
                        (org-list
                         (when encrypted "Encrypted")
                         (when linked "Linked"))))

              (when timestamps
                (concat "*Timestamps*" (apply #'org-list timestamps)))

              (when relations
                (concat "*Relations*" (apply #'org-list (mapcar #'org-glance-relation-interpreter relations))))

              (when clocks
                (concat "*Time spent*" (apply #'org-newline clocks)))))))
         (condition-case nil
             (org-update-checkbox-count-maybe 'all)
           (error nil))
         (buffer-string))))))

(cl-defmacro org-glance:with-headline-at-point (&rest forms)
  `(save-excursion
     (org-glance-headline:search-parents)
     (unless (org-glance-headline-p)
       (error "Unable to find headline at point"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(cl-defun org-glance-headline-reference (&optional (type 'org-glance-visit))
  (org-glance:with-headline-at-point
   (let* ((id (org-glance-headline:id))
          (state (org-glance-headline:state))
          (alias (-some->> (org-glance-headline:alias)
                   (replace-regexp-in-string (format "^%s[[:space:]]*" state) "")))
          (tags (org-glance-headline:tags))
          (class (s-join ", " (cl-loop
                                 for tag in tags
                                 collect (format "[[org-glance-overview:%s][%s]]" (downcase tag) tag)))))
     (format "%s%s [[%s:%s][%s]]"
             (if (string-empty-p state) "" (format "[[org-glance-state:%s][%s]] " state state))
             class
             type
             id
             alias))))

(org-glance:provide)
