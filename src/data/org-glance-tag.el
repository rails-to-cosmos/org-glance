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

(cl-defun org-glance-tag:from-string (value)
  "Coerce VALUE (a tag string, or any printable) to its canonical tag symbol.
Trimmed, downcased and interned, so mixed-case or padded input yields the same
downcased symbol the rest of the system stores and compares (`org-glance-tag?').
The inverse of `org-glance-tag:to-string'; used wherever a tag enters from text
\(filter specs, captured/parsed headlines, the picker, tag-config)."
  (->> value (format "%s") s-trim downcase intern))

(provide 'org-glance-tag)
