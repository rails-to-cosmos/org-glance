;; -*- lexical-binding: t -*-

(require 'dash)
(require 'org)
(require 'org-element)

(require 'org-glance-utils)
(require 'org-glance-tag)

(defvar org-glance:key-value-pair-re)

(declare-function org-glance--back-to-heading "org-glance-utils.el")
(declare-function org-glance--parse-links "org-glance-utils.el")
(declare-function org-glance--with-file-visited "org-glance-utils.el")

(declare-function org-glance-tag:from-string "org-glance-tag.el" (value))
(declare-function org-glance-exception:headline-not-found "org-glance-exceptions.el")

(defconst org-glance-headline:spec `((:raw-value   . (:reader org-glance-headline:plain-title  :writer org-glance-headline:plain-title))
                                     (:begin       . (:reader org-glance-headline:begin        :writer org-glance-headline:begin))
                                     (:file        . (:reader org-glance-headline:file-name    :writer org-glance-headline:file-name))
                                     (:commentedp  . (:reader org-glance-headline:commented?   :writer org-glance-headline:commented?))
                                     (:archivedp   . (:reader org-glance-headline:archived?    :writer org-glance-headline:archived?))
                                     (:linked      . (:reader org-glance-headline:linked?      :writer org-glance-element:linked?))
                                     (:propertized . (:reader org-glance-headline:propertized? :writer org-glance-element:propertized?))
                                     (:encrypted   . (:reader org-glance-headline:encrypted?   :writer org-glance-element:encrypted?))
                                     (:buffer      . (:reader org-glance-headline:buffer       :writer org-glance-element:buffer))
                                     (:closed      . (:reader org-glance-headline:closed?      :writer org-glance-headline:closed?)))
  "Map `org-element-property' to `org-glance' extractor method.")

(cl-defmacro org-glance-headline:with-element-narrowing (element &rest forms)
  (declare (indent 1) (debug t))
  `(save-excursion
     (save-restriction
       (goto-char (org-element-property :begin element))
       (org-narrow-to-element)
       ,@forms)))

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  `(save-excursion
     (org-glance-headline:search-parents)
     (unless (org-glance-headline? (org-glance-headline:at-point))
       (error "Unable to find headline at point"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(cl-defmacro org-glance-headline:with-narrowed-headline (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup))
         (id (org-glance-headline:id ,headline))
         (file (org-glance-headline:file-name ,headline))
         (buffer (org-glance-headline:buffer ,headline)))
     (cond (file (org-glance--with-file-visited file
                   (org-glance-headline:search-buffer-by-id id)
                   (org-glance-headline:with-headline-at-point ,@forms)))
           ((and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (org-glance-headline:search-buffer-by-id id)
              (org-glance-headline:with-headline-at-point ,@forms)))
           (t (org-glance-exception:headline-not-found (prin1-to-string ,headline))))))

(cl-defun org-glance-headline:buffer-positions (id)
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (element) (when (string= (org-glance-headline:id element) id)
                   (org-element-property :begin element)))))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((positions (org-glance-headline:buffer-positions id)))
    (unless positions
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" (buffer-file-name) id))

    (when (> (length positions) 1)
      (message "Headline ID %s is not unique in file %s" id (buffer-file-name)))

    (goto-char (car positions))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:update (element &rest properties)
  "Enrich `org-element' ELEMENT with PROPERTIES."
  (cl-loop for (key value) on properties by #'cddr
           do (org-element-put-property element key value)
           finally (return element)))

(cl-defun org-glance-headline:serialize (headline)
  (cl-loop for (_ . methods) in org-glance-headline:spec
           for reader = (plist-get methods :reader)
           collect (funcall reader headline)))

(cl-defun org-glance-headline:deserialize (value)
  (cl-loop with element = (org-element-create 'headline)
           for (property . _) in org-glance-headline:spec
           for index from 0
           do (org-glance-headline:update element property (nth index value))
           finally (return element)))

(cl-defun org-glance-element:linked? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (not (null (org-glance--parse-links))))))

(cl-defun org-glance-element:propertized? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (org-end-of-meta-data t)
         (not (null (re-search-forward org-glance:key-value-pair-re nil t))))))

(cl-defun org-glance-element:encrypted? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (org-end-of-meta-data t)
         (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-element:buffer (element)
  (when-let (headline (org-glance-headline? element))
    (condition-case nil
        (buffer-name (get-file-buffer (org-glance-headline:file-name headline)))
      (wrong-type-argument nil))))

(cl-defun org-glance-headline? (element)
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property specified.
Return headline or nil if it is not a proper `org-glance-headline'."
  (when (org-element-property :ORG_GLANCE_ID element)
    element))

(cl-defun org-glance-headline:title (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:begin (headline)
  (org-element-property :begin headline))

(cl-defun org-glance-headline:end (headline)
  (org-element-property :contents-end headline))

(cl-defun org-glance-headline:tags (headline)
  (mapcar #'org-glance-tag:from-string (org-element-property :tags headline)))

(cl-defun org-glance-headline:encrypted? (headline)
  (not (null (org-element-property :encrypted headline))))

(cl-defun org-glance-headline:linked? (headline)
  (not (null (org-element-property :linked headline))))

(cl-defun org-glance-headline:propertized? (headline)
  (not (null (org-element-property :propertized headline))))

(cl-defun org-glance-headline:active? (headline)
  (and (org-glance-headline? headline)
       (not (org-glance-headline:done? headline))
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))
       (not (org-glance-headline:closed? headline))))

(cl-defun org-glance-headline:id (headline)
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (headline)
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (headline)
  (not (null (org-element-property :commentedp headline))))

(cl-defun org-glance-headline:archived? (headline)
  (not (null (org-element-property :archivedp headline))))

(cl-defun org-glance-headline:closed? (headline)
  (not (null (org-element-property :closed headline))))

(cl-defun org-glance-headline:done? (headline)
  (member (org-glance-headline:state headline) org-done-keywords-for-agenda))

(cl-defun org-glance-headline:alias (headline)
  "Get title of HEADLINE considering alias property."
  (or (org-element-property :ALIAS headline)
      (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:priority (headline)
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (headline)
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:file-name (headline)
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:modtime (headline)
  (-some-> headline
    org-glance-headline:file-name
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:level (headline)
  (org-element-property :level headline))

(cl-defun org-glance-headline:buffer (headline)
  (let ((buffer (org-element-property :buffer headline)))
    (cond ((bufferp buffer) (buffer-name buffer))
          (t buffer))))

(cl-defun org-glance-headline:schedule (headline)
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (headline)
  (org-element-property :deadline headline))

;; TODO check it for correctness
(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance--back-to-heading)

  (while (not (or ;; (org-glance-headline? (org-element-at-point))
               (org-before-first-heading-p)
               (bobp)))
    (org-up-heading-or-point-min))

  (-some-> (org-element-at-point)
    (org-glance-headline?)
    (org-glance-headline:from-element)))

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (save-excursion
    (when (condition-case nil
              (or (org-at-heading-p) (org-back-to-heading))
            (error nil))
      (org-glance-headline:search-parents))))

(cl-defun org-glance-headline:from-element (element)
  (when (eql 'headline (org-element-type element))
    (cl-loop for (property . methods) in org-glance-headline:spec
             for index from 0
             for writer = (plist-get methods :writer)
             for value = (funcall writer element)
             do (org-glance-headline:update element property value)
             finally (return element))))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance--back-to-heading)
  (while (and (org-glance-headline? (org-glance-headline:at-point)) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:replace-headline-at-point (contents)
  (let ((beg (org-glance-headline:begin (org-glance-headline:at-point)))
        (end (save-excursion (org-end-of-subtree t)))
        (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert contents)))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:contents (headline)
  "Extracts HEADLINE contents.
FIXME. Unstable one. Refactor is needed."
  (let ((file (org-glance-headline:file-name headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
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
          (t (org-glance-exception:headline-not-found "Unable to determine headline location")))))

(cl-defun org-glance-headline:hash (headline)
  (let ((contents (org-glance-headline:contents headline)))
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:plain-title (headline)
  (with-temp-buffer
    (save-excursion (insert (org-glance-headline:title headline)))
    (org-glance--remove-links 'org-glance-overview 'org-glance-state)
    (org-glance--substitute-links)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:add-log-note (string &rest objects)
  (org-glance-headline:with-headline-at-point
   (goto-char (org-log-beginning t))
   (insert (apply #'format string objects) "\n")))

(iter-defun org-glance-headline:forward-iterator ()
  "Iterate over buffer headlines from top to bottom."
  (if-let ((headline (org-glance-headline:from-element (org-element-at-point))))
      (iter-yield headline)
    (outline-previous-heading))

  (while (not (eobp))
    (when-let ((headline (org-glance-headline:from-element (org-element-at-point))))
      (iter-yield headline))
    (outline-next-heading)))

(cl-defun org-glance-headline:test-iter ()
  (interactive)
  (let ((it (org-glance-headline:forward-iterator)))
    (while-let ((headline (iter-next it)))
      (message (org-glance-headline:id headline)))))

(provide 'org-glance-headline)
