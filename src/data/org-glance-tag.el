;; -*- lexical-binding: t -*-
;; `org-glance-tag' data model

(require 'org)
(require 's)
(require 'dash)
(require 'cl-lib)

(cl-defun org-glance-tag? (tag)
  "Return t if TAG is a downcased, non-nil symbol."
  (and (symbolp tag)
       (not (null tag))
       (string= (symbol-name tag) (downcase (symbol-name tag)))))

(cl-deftype org-glance-tag () '(satisfies org-glance-tag?))

(cl-defun org-glance-tag:to-string (tag)
  (cl-check-type tag org-glance-tag)
  (symbol-name tag))

(cl-defun org-glance-tag:as-list (tags)
  "Coerce TAGS (a tag symbol or a list of tag symbols) to a list."
  (if (listp tags) tags (list tags)))

(cl-defun org-glance-tag:validate-string (value)
  "Return VALUE trimmed when org can parse it as a tag; `user-error' otherwise.
Checks `org-tag-re'; callers are tag CREATION boundaries only (invariant 13)."
  (let ((s (s-trim (format "%s" value))))
    (unless (string-match-p (format "\\`%s\\'" org-tag-re) s)
      (user-error "`%s' is not a valid org tag (allowed: letters, digits, _ @ # %%)" s))
    s))

(cl-defun org-glance-tag:from-string (value)
  "Coerce VALUE (a tag string, or any printable) to its canonical tag symbol.
Trim, downcase and intern it; the inverse of `org-glance-tag:to-string'."
  (->> value (format "%s") s-trim downcase intern))

(provide 'org-glance-tag)
