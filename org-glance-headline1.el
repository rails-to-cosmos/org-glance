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
  (-encrypted? nil :read-only t :type bool)
  (-links nil :read-only t :type list)
  (-properties nil :read-only t :type list))

(cl-defun org-glance-headline1:at-point ()
  (save-excursion
    (org-glance--back-to-heading)
    (cl-do ((element (org-element-at-point) (org-element-at-point)))
        ;; until:
        ((or (and (listp element) (eq (car element) 'headline)) (org-before-first-heading-p) (bobp))
         ;; return:
         (when (and (listp element) (eq (car element) 'headline))
           (org-glance-headline1--from-element element)))
      ;; do:
      (org-up-heading-or-point-min))))

(cl-defun org-glance-headline1:active? (headline)
  (and (not (org-glance-headline1:done? headline))
       (not (org-glance-headline1:commented? headline))
       (not (org-glance-headline1:archived? headline))
       (not (org-glance-headline1:closed? headline))))

(cl-defun org-glance-headline1:encrypted? (headline)
  (thunk-force (org-glance-headline1:-encrypted? headline)))

(cl-defun org-glance-headline1:links (headline)
  (thunk-force (org-glance-headline1:-links headline)))

(cl-defun org-glance-headline1:properties (headline)
  (thunk-force (org-glance-headline1:-properties headline)))

(cl-defun org-glance-headline1:get-user-property (property headline)
  (alist-get property (org-glance-headline1:properties headline) nil nil #'string=))

(cl-defun org-glance-headline1:done? (headline)
  (not (null (member (org-glance-headline1:state headline) org-done-keywords))))

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
      (make-org-glance-headline1
       :id id
       :title title
       :tags tags
       :hash hash
       :state state
       :contents contents
       :archived? archived?
       :commented? commented?
       :closed? closed?
       :-links (thunk-delay
                (with-temp-buffer
                  (org-mode)
                  (insert contents)
                  (org-glance--parse-links)))
       :-properties (thunk-delay
                     (with-temp-buffer
                       (org-mode)
                       (insert contents)
                       (org-glance--buffer-key-value-pairs)))
       :-encrypted? (thunk-delay
                     (with-temp-buffer
                       (org-mode)
                       (insert contents)
                       (goto-char (point-min))
                       (org-end-of-meta-data t)
                       (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")))))))))

(provide 'org-glance-headline1)
