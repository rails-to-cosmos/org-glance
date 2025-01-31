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

(unless (fboundp 'org-element-type-p)
  (cl-defun org-element-type-p (node types)
    (cl-typecase node
      (list (member (car node) (cl-typecase types
                                 (list types)
                                 (symbol (list types)))))
      (otherwise nil))))

(defconst org-glance-headline-v2:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")
(defconst org-glance-headline-v2:hash-ignore-properties (list "ORG_GLANCE_ID" "ORG_GLANCE_HASH"))

(cl-defstruct (org-glance-headline-v2 (:predicate org-glance-headline-v2?)
                                      (:conc-name org-glance-headline-v2:))
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

  ;; Lazy attributes start with "-". Each has a builder: `org-glance-headline-v2--<slot-name>'
  (-hash nil :read-only t :type (or string function))
  (-encrypted? nil :read-only t :type (or bool function))
  (-links nil :read-only t :type (or list function))
  (-properties nil :read-only t :type (or list function)))

(cl-defmacro org-glance-headline-v2:with-contents (contents &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (insert (cl-typecase ,contents
               (org-glance-headline-v2 (org-glance-headline-v2:contents ,contents))
               (string ,contents)
               (otherwise (error "Expected `org-glance-headline-v2' or string, but got %s" (type-of ,contents)))))
     (goto-char (point-min))
     ,@forms))

(cl-defun org-glance-headline-v2:at-point ()
  (save-excursion
    (cl-loop initially (or (org-at-heading-p) (org-back-to-heading-or-point-min))
             while (org-at-heading-p)
             for element = (org-element-at-point)
             if (org-element-type-p element 'headline) ;; (and (listp element) (eq (car element) 'headline))
             return (org-glance-headline-v2--from-element element)
             else if (or (org-before-first-heading-p) (bobp))
             do (error "Unable to find `org-glance-headline-v2' at point")
             else
             do (org-up-heading-or-point-min))))

(cl-defun org-glance-headline-v2:active? (headline)
  (not (or (org-glance-headline-v2:done? headline)
           (org-glance-headline-v2:commented? headline)
           (org-glance-headline-v2:archived? headline)
           (org-glance-headline-v2:closed headline))))

(cl-defun org-glance-headline-v2:encrypted? (headline)
  (let ((encrypted? (org-glance-headline-v2:-encrypted? headline)))
    (cl-typecase encrypted?
      (boolean encrypted?)
      (function (thunk-force encrypted?))
      (otherwise (error "Lazy evaluation failed: `org-glance-headline-v2:encrypted?'")))))

(cl-defun org-glance-headline-v2:hash (headline)
  (thunk-force (org-glance-headline-v2:-hash headline)))

(cl-defun org-glance-headline-v2:links (headline)
  (thunk-force (org-glance-headline-v2:-links headline)))

(cl-defun org-glance-headline-v2:properties (headline)
  (thunk-force (org-glance-headline-v2:-properties headline)))

(cl-defun org-glance-headline-v2:get-user-property (property headline)
  (alist-get property (org-glance-headline-v2:properties headline) nil nil #'string=))

(cl-defun org-glance-headline-v2:done? (headline)
  (not (null (member (org-glance-headline-v2:state headline) org-done-keywords))))

(cl-defun org-glance-headline-v2--hash (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline-v2:with-contents contents
                 (cl-loop initially (org-mode)
                          for property in org-glance-headline-v2:hash-ignore-properties
                          do (org-entry-delete nil property)
                          finally return (let ((data (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
                                           (with-temp-buffer
                                             (insert data)
                                             (buffer-hash)))))))

(cl-defun org-glance-headline-v2--links (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline-v2:with-contents contents
                 (org-glance--parse-links))))

(cl-defun org-glance-headline-v2--properties (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline-v2:with-contents contents
                 (cl-loop while (re-search-forward org-glance-headline-v2:key-value-pair-re nil t)
                          collect (cons (s-trim (substring-no-properties (match-string 1)))
                                        (s-trim (substring-no-properties (match-string 2))))))))

(cl-defun org-glance-headline-v2--encrypted (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline-v2:with-contents contents
                 (org-end-of-meta-data t)
                 (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-headline-v2--from-string (contents)
  (cl-check-type contents string)
  (org-glance-headline-v2:with-contents contents
    (org-mode)
    (unless (or (org-at-heading-p) (re-search-forward org-heading-regexp nil t))
      (error "Unable to find `org-element' of type `headline' in the provided contents"))
    (org-glance-headline-v2:at-point)))

(cl-defun org-glance-headline-v2--from-lines (&rest lines)
  (declare (indent 0))
  (cl-check-type lines list)
  (org-glance-headline-v2--from-string (apply (lambda (&rest tokens) (s-join "\n" tokens)) lines)))

(cl-defun org-glance-headline-v2--from-element (element)
  "Create `org-glance-headline-v2' from `org-element' ELEMENT."
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
                   ""))
        (contents (let ((buffer (or (org-element-property :buffer element) (current-buffer)))
                        (begin (org-element-property :begin element))
                        (end (org-element-property :end element)))
                    (with-current-buffer buffer
                      (buffer-substring-no-properties begin end)))))
    (make-org-glance-headline-v2 :id id
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
                                 :-hash (org-glance-headline-v2--hash contents)
                                 :-links (org-glance-headline-v2--links contents)
                                 :-properties (org-glance-headline-v2--properties contents)
                                 :-encrypted? (org-glance-headline-v2--encrypted contents))))

(cl-defun org-glance-headline-v2--copy (headline &rest update-plist)
  "Copy HEADLINE but replace slot values described in UPDATE-PLIST."
  (declare (indent 1))
  (cl-check-type headline org-glance-headline-v2)
  (cl-loop for slot-info in (cdr (cl-struct-slot-info 'org-glance-headline-v2))
           for slot-name = (car slot-info)
           for slot-property = (intern (format ":%s" slot-name))
           for slot-value = (if (plist-member update-plist slot-property)
                                (plist-get update-plist slot-property)
                              (cl-struct-slot-value 'org-glance-headline-v2 slot-name headline))
           collect slot-property into params
           collect slot-value into params
           finally (return (apply #'make-org-glance-headline-v2 params))))

(cl-defun org-glance-headline-v2:encrypt (headline password)
  (cl-check-type headline org-glance-headline-v2)
  (cl-check-type password string)
  (if (org-glance-headline-v2:encrypted? headline)
      headline
    (org-glance-headline-v2--copy headline
      :contents (org-glance-headline-v2:with-contents headline
                  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                        (end (save-excursion (org-end-of-subtree t) (point))))
                    (org-glance--encrypt-region beg end password)
                    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
      :-encrypted? t)))

(cl-defun org-glance-headline-v2:decrypt (headline password)
  (cl-check-type headline org-glance-headline-v2)
  (cl-check-type password string)
  (if (not (org-glance-headline-v2:encrypted? headline))
      headline
    (org-glance-headline-v2--copy headline
      :contents (org-glance-headline-v2:with-contents headline
                  (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                        (end (save-excursion (org-end-of-subtree t) (point))))
                    (org-glance--decrypt-region beg end password)
                    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
      :-encrypted? nil)))

(cl-defun org-glance-headline-v2:search-forward (id)
  (cl-check-type id string)
  (save-excursion
    (cl-loop while (re-search-forward (concat ":ORG_GLANCE_ID:[ \t]+" id) nil 'no-error)
             for headline = (org-glance-headline-v2:at-point)
             when (and (org-glance-headline-v2? headline)
                       (string= id (org-glance-headline-v2:id headline)))
             return headline)))

(cl-defun org-glance-headline-v2:reset-indent (headline)
  (cl-check-type headline org-glance-headline-v2)
  (let ((contents (org-glance-headline-v2:with-contents headline
                    (org-mode)
                    (re-search-forward "\\^*")
                    (while (looking-at "^\\*\\*")
                      (org-promote-subtree))
                    (s-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (org-glance-headline-v2--copy headline
      :indent 1
      :contents contents
      :-hash (org-glance-headline-v2--hash contents))))

(cl-defun org-glance-headline-v2:title-clean (headline)
  (cl-check-type headline org-glance-headline-v2)
  (replace-regexp-in-string
   org-link-bracket-re
   (lambda (match) (or (match-string 2 match) (match-string 1 match) ""))
   (org-glance-headline-v2:title headline)))

(cl-defun org-glance-headline-v2:add-note (headline message &rest format-args)
  (cl-check-type headline org-glance-headline-v2)
  (cl-check-type message string)
  (let ((contents (org-glance-headline-v2:with-contents headline
                    (org-mode)
                    (goto-char (org-log-beginning t))
                    (insert "- " (apply #'format message format-args) "\n")
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (org-glance-headline-v2--copy headline
      :contents contents
      :-hash (org-glance-headline-v2--hash contents))))

(cl-defun org-glance--element-timestamps (element)
  (cl-check-type element list)
  (cl-case (org-element-type element)
    (timestamp (list element))
    ;; (headline (list (org-element-property :scheduled element)
    ;;                 (org-element-property :deadline element)))
    ))

(cl-defun org-glance-headline-v2:timestamps (headline)
  (cl-check-type headline org-glance-headline-v2)
  (org-glance-headline-v2:with-contents headline
    (org-mode)
    (->> #'org-glance--element-timestamps
         (org-element-map (org-element-parse-buffer) '(timestamp headline))
         (-flatten-n 1)
         (-non-nil))))

(cl-defun org-glance-headline-v2:timestamps-raw (headline)
  (cl-check-type headline org-glance-headline-v2)
  (mapcar (-partial #'org-element-property :raw-value) (org-glance-headline-v2:timestamps headline)))

(cl-defun org-glance-headline-v2:clocks (headline)
  (cl-check-type headline org-glance-headline-v2)
  (org-glance-headline-v2:with-contents headline
    (org-mode)
    (cl-loop while (re-search-forward org-clock-line-re nil t)
             when (org-at-clock-log-p)
             collect (org-element-at-point))))

(cl-defun org-glance-headline-v2:tag-string (headline)
  (cl-check-type headline org-glance-headline-v2)
  (when-let (tags (org-glance-headline-v2:tags headline))
    (-> (s-join ":" (mapcar #'symbol-name tags))
        (s-wrap ":" ":"))))

(cl-defun org-glance-headline-v2:overview (headline)
  (cl-check-type headline org-glance-headline-v2)
  (cl-flet ((org-list (&rest items) (org-glance--join-leading-separator-but-null "\n- " items))
            (org-newline (&rest items) (org-glance--join-leading-separator-but-null "\n" items)))
    (let ((timestamps (org-glance-headline-v2:timestamps-raw headline))
          (clocks (->> (org-glance-headline-v2:clocks headline)
                       (--filter (eql 'closed (org-element-property :status it)))))
          (tags (org-glance-headline-v2:tag-string headline))
          (hash (org-glance-headline-v2:hash headline))
          (state (org-glance-headline-v2:state headline))
          (id (org-glance-headline-v2:id headline))
          (title (org-glance-headline-v2:title-clean headline))
          (priority (org-glance-headline-v2:priority headline))
          (closed (org-glance-headline-v2:closed headline))
          (schedule (org-glance-headline-v2:schedule headline))
          (deadline (org-glance-headline-v2:deadline headline))
          (encrypted (org-glance-headline-v2:encrypted? headline))
          (links (org-glance-headline-v2:links headline)))
      (org-glance-headline-v2:with-contents (org-glance-headline-v2:header headline)
        (org-mode)

        (when id (org-entry-put nil "ORG_GLANCE_ID" id))
        (org-entry-put nil "ORG_GLANCE_HASH" hash)

        (goto-char (point-max))

        (insert (s-join "\n" (-non-nil (list (when timestamps (concat "\n*Timestamps*\n- " (s-join "\n- " timestamps)))
                                             (when clocks (concat "\n*Time spent*\n" (s-join "\n" (mapcar #'org-element-interpret-data clocks))))))))

        (condition-case nil
            (org-update-checkbox-count-maybe 'all)
          (error nil))

        (s-trim (buffer-substring-no-properties (point-min) (point-max)))))))

(cl-defun org-glance-headline-v2:header (headline)
  (cl-check-type headline org-glance-headline-v2)
  (org-glance-headline-v2:with-contents headline
    (org-mode)
    (buffer-substring-no-properties (point) (save-excursion
                                              (org-end-of-meta-data)
                                              (point)))))

(provide 'org-glance-headline-v2)
