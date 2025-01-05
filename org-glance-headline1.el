;; -*- lexical-binding: t -*-

(require 'dash)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'cl-macs)
(require 'thunk)

(require 'org-glance-exception)
(require 'org-glance-utils)
(require 'org-glance-tag)

(defvar org-glance:key-value-pair-re)

(declare-function org-glance--back-to-heading "org-glance-utils.el")
(declare-function org-glance--parse-links "org-glance-utils.el")
(declare-function org-glance--with-file-visited "org-glance-utils.el")
(declare-function org-glance-tag:from-string "org-glance-tag.el" (value))

(cl-defstruct (org-glance-headline1 (:predicate org-glance-headline1?)
                                    (:conc-name org-glance-headline1:))
  (archived? nil :read-only t :type bool)
  (closed? nil :read-only t :type bool)
  (commented? nil :read-only t :type bool)
  (contents nil :read-only t :type string)
  (hash nil :read-only t :type string)
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (title nil :read-only t :type string)

  ;; lazy attributes start with "-"
  (-encrypted? nil :read-only t :type (or bool function))
  (-links nil :read-only t :type (or list function))
  (-properties nil :read-only t :type (or list function)))

(cl-defun org-glance-headline1:at-point ()
  (save-excursion
    (cl-loop initially (org-glance--back-to-heading)
             while t
             for element = (org-element-at-point)
             for headline? = (and (listp element) (eq (car element) 'headline))
             if headline?
             return (org-glance-headline1--from-element element)
             else if (or (org-before-first-heading-p) (bobp))
             do (error "Unable to find `org-glance-headline1' at point")
             else
             do (org-up-heading-or-point-min))))

(cl-defun org-glance-headline1:active? (headline)
  (and (not (org-glance-headline1:done? headline))
       (not (org-glance-headline1:commented? headline))
       (not (org-glance-headline1:archived? headline))
       (not (org-glance-headline1:closed? headline))))

(cl-defun org-glance-headline1:encrypted? (headline)
  (let ((encrypted? (org-glance-headline1:-encrypted? headline)))
    (cl-typecase encrypted?
      (boolean encrypted?)
      (compiled-function (thunk-force encrypted?))
      (otherwise (error "Lazy evaluation failed: encrypted?")))))

(cl-defun org-glance-headline1:links (headline)
  (thunk-force (org-glance-headline1:-links headline)))

(cl-defun org-glance-headline1:user-properties (headline)
  (thunk-force (org-glance-headline1:-properties headline)))

(cl-defun org-glance-headline1:get-user-property (property headline)
  (alist-get property (org-glance-headline1:user-properties headline) nil nil #'string=))

(cl-defun org-glance-headline1:done? (headline)
  (not (null (member (org-glance-headline1:state headline) org-done-keywords))))

(cl-defun org-glance-headline1--links-extractor (contents)
  (thunk-delay
   (with-temp-buffer
     (insert contents)
     (org-glance--parse-links))))

(cl-defun org-glance-headline1--user-properties-extractor (contents)
  (thunk-delay
   (with-temp-buffer
     (org-mode)
     (insert contents)
     (org-glance--buffer-key-value-pairs))))

(cl-defun org-glance-headline1--encrypted-property-extractor (contents)
  (thunk-delay
   (with-temp-buffer
     (org-mode)
     (insert contents)
     (goto-char (point-min))
     (org-end-of-meta-data t)
     (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-headline1--from-element (element)
  "Create `org-glance-headline1' from `org-element' ELEMENT."
  (let ((id (org-element-property :ORG_GLANCE_ID element))
        (tags (mapcar #'org-glance-tag:from-string (org-element-property :tags element)))
        (archived? (not (null (org-element-property :archivedp element))))
        (commented? (not (null (org-element-property :commentedp element))))
        (closed? (not (null (org-element-property :closed element))))
        (state (substring-no-properties (or (org-element-property :todo-keyword element) "")))
        (title (or (org-element-property :TITLE element)
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
                                 :hash hash
                                 :state state
                                 :contents contents
                                 :archived? archived?
                                 :commented? commented?
                                 :closed? closed?
                                 :-links (org-glance-headline1--links-extractor contents)
                                 :-properties (org-glance-headline1--user-properties-extractor contents)
                                 :-encrypted? (org-glance-headline1--encrypted-property-extractor contents)))))

(cl-defun org-glance-headline1:encrypt (headline password)
  (cl-check-type headline org-glance-headline1)
  (cl-check-type password string)
  (let ((contents (with-temp-buffer
                    (org-mode)
                    (insert (org-glance-headline1:contents headline))
                    (goto-char (point-min))
                    (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                          (end (save-excursion (org-end-of-subtree t) (point))))
                      (org-glance--encrypt-region beg end password))
                    (buffer-string))))
    (make-org-glance-headline1 :id (org-glance-headline1:id headline)
                               :title (org-glance-headline1:title headline)
                               :tags (org-glance-headline1:tags headline)
                               :hash (org-glance-headline1:hash headline)
                               :state (org-glance-headline1:state headline)
                               :contents contents
                               :archived? (org-glance-headline1:archived? headline)
                               :commented? (org-glance-headline1:commented? headline)
                               :closed? (org-glance-headline1:closed? headline)
                               :-links (org-glance-headline1:-links headline)
                               :-properties (org-glance-headline1:-properties headline)
                               :-encrypted? t)))

(cl-defun org-glance-headline1:decrypt (headline password)
  (cl-check-type headline org-glance-headline1)
  (cl-check-type password string)
  (let ((contents (with-temp-buffer
                    (org-mode)
                    (insert (org-glance-headline1:contents headline))
                    (goto-char (point-min))
                    (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                          (end (save-excursion (org-end-of-subtree t) (point))))
                      (org-glance--decrypt-region beg end password))
                    (buffer-string))))
    (make-org-glance-headline1 :id (org-glance-headline1:id headline)
                               :title (org-glance-headline1:title headline)
                               :tags (org-glance-headline1:tags headline)
                               :hash (org-glance-headline1:hash headline)
                               :state (org-glance-headline1:state headline)
                               :contents contents
                               :archived? (org-glance-headline1:archived? headline)
                               :commented? (org-glance-headline1:commented? headline)
                               :closed? (org-glance-headline1:closed? headline)
                               :-links (org-glance-headline1:-links headline)
                               :-properties (org-glance-headline1:-properties headline)
                               :-encrypted? nil)))

(provide 'org-glance-headline1)
