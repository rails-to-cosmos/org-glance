;; -*- lexical-binding: t -*-

(require 's)
(require 'dash)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'cl-macs)
(require 'thunk)
(require 'org-clock)

(require 'org-glance-exception)
(require 'org-glance-utils)
(require 'org-glance-tag)
(require 'org-glance-datetime-mode)

(declare-function org-glance--back-to-heading "org-glance-utils.el")
(declare-function org-glance--parse-links "org-glance-utils.el")
(declare-function org-glance--with-file-visited "org-glance-utils.el")

(cl-defstruct (org-glance-headline1 (:predicate org-glance-headline1?)
                                    (:conc-name org-glance-headline1:))
  (contents nil :read-only t :type string)
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (title nil :read-only t :type string)
  (priority nil :read-only t :type number)
  (indent nil :read-only t :type number)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)

  ;; Metadata
  (archived? nil :read-only t :type bool)
  (closed nil :read-only t :type (or null string))
  (commented? nil :read-only t :type bool)

  ;; Lazy attributes start with "-". Each has a `org-glance-headline1--<slot-name>-lazy' builder
  (-hash nil :read-only t :type (or string function))
  (-encrypted? nil :read-only t :type (or bool function))
  (-links nil :read-only t :type (or list function))
  (-properties nil :read-only t :type (or list function)))

(cl-defun org-glance-headline1:at-point ()
  (save-excursion
    (cl-loop initially (or (org-at-heading-p) (org-back-to-heading-or-point-min))
             while (org-at-heading-p)
             for element = (org-element-at-point)
             if (and (listp element) (eq (car element) 'headline))  ;; if (org-element-type-p element 'headline)
             return (org-glance-headline1--from-element element)
             else if (or (org-before-first-heading-p) (bobp))
             do (error "Unable to find `org-glance-headline1' at point")
             else
             do (org-up-heading-or-point-min))))

(cl-defun org-glance-headline1:active? (headline)
  (and (not (org-glance-headline1:done? headline))
       (not (org-glance-headline1:commented? headline))
       (not (org-glance-headline1:archived? headline))
       (not (org-glance-headline1:closed headline))))

(cl-defun org-glance-headline1:encrypted? (headline)
  (let ((encrypted? (org-glance-headline1:-encrypted? headline)))
    (cl-typecase encrypted?
      (boolean encrypted?)
      (function (thunk-force encrypted?))
      (otherwise (error "Lazy evaluation failed: encrypted?")))))

(cl-defun org-glance-headline1:hash (headline)
  (thunk-force (org-glance-headline1:-hash headline)))

(cl-defun org-glance-headline1:links (headline)
  (thunk-force (org-glance-headline1:-links headline)))

(cl-defun org-glance-headline1:user-properties (headline)
  (thunk-force (org-glance-headline1:-properties headline)))

(cl-defun org-glance-headline1:get-user-property (property headline)
  (alist-get property (org-glance-headline1:user-properties headline) nil nil #'string=))

(cl-defun org-glance-headline1:done? (headline)
  (not (null (member (org-glance-headline1:state headline) org-done-keywords))))

(cl-defun org-glance-headline1--hash-lazy (contents)
  (cl-check-type contents string)
  (thunk-delay
   (with-temp-buffer
     (insert contents)
     (buffer-hash))))

(cl-defun org-glance-headline1--links-lazy (contents)
  (cl-check-type contents string)
  (thunk-delay
   (with-temp-buffer
     (insert contents)
     (org-glance--parse-links))))

(cl-defun org-glance-headline1--properties-lazy (contents)
  (cl-check-type contents string)
  (thunk-delay
   (with-temp-buffer
     (insert contents)
     (org-glance--buffer-key-value-pairs))))

(cl-defun org-glance-headline1--encrypted-lazy (contents)
  (cl-check-type contents string)
  (thunk-delay
   (with-temp-buffer
     (insert contents)
     (goto-char (point-min))
     (org-end-of-meta-data t)
     (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-headline1--from-string (contents)
  (cl-check-type contents string)
  (with-temp-buffer
    (insert contents)
    (org-mode)
    (goto-char (point-min))
    (or (org-at-heading-p) (progn (re-search-forward org-heading-regexp)))
    (org-glance-headline1:at-point)))

(cl-defun org-glance-headline1--from-lines (&rest lines)
  (declare (indent 0))
  (cl-check-type lines list)
  (org-glance-headline1--from-string (apply (lambda (&rest tokens) (s-join "\n" tokens)) lines)))

(cl-defun org-glance-headline1--from-element (element)
  "Create `org-glance-headline1' from `org-element' ELEMENT."
  (let ((id (org-element-property :ORG_GLANCE_ID element))
        (tags (mapcar (-compose #'intern #'s-downcase) (org-element-property :tags element)))
        (archived? (not (null (org-element-property :archivedp element))))
        (commented? (not (null (org-element-property :commentedp element))))
        (closed (org-element-property :closed element))
        (state (substring-no-properties (or (org-element-property :todo-keyword element) "")))
        (priority (org-element-property :priority element))
        (indent (or (org-element-property :level element) 1))
        (schedule (org-element-property :scheduled element))
        (deadline (org-element-property :deadline element))
        (title (or (org-element-property :ORG_GLANCE_TITLE element)
                   (org-element-property :TITLE element)
                   (org-element-property :raw-value element)
                   "")))

    (cl-destructuring-bind (contents hash)
        (let ((buffer (or (org-element-property :buffer element) (current-buffer)))
              (begin (org-element-property :begin element))
              (end (org-element-property :end element)))
          (with-current-buffer buffer
            (save-restriction
              (narrow-to-region begin end)
              (list (buffer-substring-no-properties (point-min) (point-max))
                    (buffer-hash)))))

      (make-org-glance-headline1 :id id
                                 :title title
                                 :tags tags
                                 :state state
                                 :priority priority
                                 :indent indent
                                 :schedule schedule
                                 :deadline deadline
                                 :contents contents
                                 :archived? archived?
                                 :commented? commented?
                                 :closed closed
                                 :-hash (org-glance-headline1--hash-lazy contents)
                                 :-links (org-glance-headline1--links-lazy contents)
                                 :-properties (org-glance-headline1--properties-lazy contents)
                                 :-encrypted? (org-glance-headline1--encrypted-lazy contents)))))

(cl-defun org-glance-headline1--copy (headline &rest update-plist)
  "Copy HEADLINE, replace slot values described in UPDATE-PLIST."
  (declare (indent 1))
  (cl-check-type headline org-glance-headline1)
  (cl-loop for slot-info in (cdr (cl-struct-slot-info 'org-glance-headline1))
           for slot-name = (car slot-info)
           for slot-property = (intern (format ":%s" slot-name))
           for slot-value = (if (plist-member update-plist slot-property)
                                (plist-get update-plist slot-property)
                              (cl-struct-slot-value 'org-glance-headline1 slot-name headline))
           collect slot-property into params
           collect slot-value into params
           finally (return (apply #'make-org-glance-headline1 params))))

(cl-defun org-glance-headline1:encrypt (headline password)
  (cl-check-type headline org-glance-headline1)
  (cl-check-type password string)
  (if (org-glance-headline1:encrypted? headline)
      headline
    (org-glance-headline1--copy headline
      :contents (with-temp-buffer
                  (insert (org-glance-headline1:contents headline))
                  (goto-char (point-min))
                  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                        (end (save-excursion (org-end-of-subtree t) (point))))
                    (org-glance--encrypt-region beg end password))
                  (buffer-string))
      :-encrypted? t)))

(cl-defun org-glance-headline1:decrypt (headline password)
  (cl-check-type headline org-glance-headline1)
  (cl-check-type password string)
  (if (not (org-glance-headline1:encrypted? headline))
      headline
    (org-glance-headline1--copy headline
      :contents (with-temp-buffer
                  (insert (org-glance-headline1:contents headline))
                  (goto-char (point-min))
                  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                        (end (save-excursion (org-end-of-subtree t) (point))))
                    (org-glance--decrypt-region beg end password))
                  (buffer-string))
      :-encrypted? nil)))

(cl-defun org-glance-headline1:search-forward (id)
  (cl-check-type id string)
  (save-excursion
    (cl-loop while (search-forward id nil 'no-error)
             for headline = (org-glance-headline1:at-point)
             when (and (org-glance-headline1? headline)
                       (string= id (org-glance-headline1:id headline)))
             return headline)))

(cl-defun org-glance-headline1:reset-indent (headline)
  (cl-check-type headline org-glance-headline1)
  (let ((contents (with-temp-buffer
                    (org-mode)
                    (insert (org-glance-headline1:contents headline))
                    (goto-char (point-min))
                    (re-search-forward "\\^*")
                    (while (looking-at "^\\*\\*")
                      (org-promote-subtree))
                    (buffer-string))))
    (org-glance-headline1--copy headline
      :indent 1
      :contents contents
      :-hash (org-glance-headline1--hash-lazy contents))))

(cl-defun org-glance-headline1:title-clean (headline)
  (cl-check-type headline org-glance-headline1)
  (replace-regexp-in-string
   org-link-bracket-re
   (lambda (match) (or (match-string 2 match) (match-string 1 match) ""))
   (org-glance-headline1:title headline)))

(cl-defun org-glance-headline1:add-note (headline message &rest format-args)
  (cl-check-type headline org-glance-headline1)
  (cl-check-type message string)

  (let ((contents (with-temp-buffer
                    (org-mode)
                    (insert (org-glance-headline1:contents headline))
                    (goto-char (point-min))
                    (goto-char (org-log-beginning t))
                    (insert "- " (apply #'format message format-args) "\n")
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (org-glance-headline1--copy headline
      :contents contents
      :-hash (org-glance-headline1--hash-lazy contents))))

(cl-defun org-glance-headline1:timestamps (headline)
  (cl-check-type headline org-glance-headline1)
  (with-temp-buffer
    (insert (org-glance-headline1:contents headline))
    (cl-loop for timestamp in (-some->> (org-glance-datetime-headline-timestamps)
                                (org-glance-datetime-filter-active)
                                (org-glance-datetime-sort-timestamps))
             collect (org-element-property :raw-value timestamp))))

(cl-defun org-glance-headline1:clocks (headline)
  (cl-check-type headline org-glance-headline1)
  (with-temp-buffer
    (insert (org-glance-headline1:contents headline))
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-clock-line-re nil t)
             when (org-at-clock-log-p)
             collect (org-element-at-point))))

(cl-defun org-glance-headline1:tag-string (headline)
  (cl-check-type headline org-glance-headline1)
  (when-let (tags (org-glance-headline1:tags headline))
    (-> (s-join ":" (mapcar #'symbol-name tags))
        (s-wrap ":" ":"))))

(cl-defun org-glance-headline1:overview (headline)
  (cl-check-type headline org-glance-headline1)
  (cl-flet ((org-list (&rest items) (org-glance--join-leading-separator-but-null "\n- " items))
            (org-newline (&rest items) (org-glance--join-leading-separator-but-null "\n" items)))
    (let ((timestamps (org-glance-headline1:timestamps headline))
          (clocks (org-glance-headline1:clocks headline))
          ;; (relations (org-glance-headline-relations))
          (tags (org-glance-headline1:tag-string headline))
          (state (org-glance-headline1:state headline))
          (id (org-glance-headline1:id headline))
          (title (org-glance-headline1:title-clean headline))
          (priority (org-glance-headline1:priority headline))
          (closed (org-glance-headline1:closed headline))
          (schedule (org-glance-headline1:schedule headline))
          (deadline (org-glance-headline1:deadline headline))
          (encrypted (org-glance-headline1:encrypted? headline))
          (links (org-glance-headline1:links headline)))
      (with-temp-buffer
        (org-mode)
        (insert (org-glance-headline1:header headline))

        (when timestamps
          (insert "\n*Timestamps*" (apply #'org-list timestamps)))

        (when clocks
          (insert "\n*Time spent*" (apply #'org-newline (mapcar #'org-element-interpret-data
                                                                (--filter (eql 'closed (org-element-property :status it)) clocks)))))

        ;; (when relations
        ;;   (concat "*Relations*" (apply #'org-list (mapcar #'org-glance-relation-interpreter relations))))

        (condition-case nil
            (org-update-checkbox-count-maybe 'all)
          (error nil))

        (s-trim (buffer-substring-no-properties (point-min) (point-max)))))))

(cl-defun org-glance-headline1:header (headline)
  (cl-check-type headline org-glance-headline1)
  (with-temp-buffer
    (insert (org-glance-headline1:contents headline))
    (goto-char (point-min))
    (org-mode)
    (buffer-substring-no-properties (point) (save-excursion
                                              (org-end-of-meta-data)
                                              (point)))))

(provide 'org-glance-headline1)
