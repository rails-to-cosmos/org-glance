;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-macs)
(require 'org-capture)
(require 's)

(require 'org-glance-tag)
(require 'org-glance-headline)
(require 'org-glance-graph)

;; Defined in org-glance.el, which requires this file (cycle) -- runtime-only refs.
(declare-function org-glance-initialized? "org-glance")

(defvar org-glance-graph)

(cl-defun org-glance-capture:--format-tags (tags)
  "Format TAGS (a symbol or list of symbols) as an org tag string `:a:b:'."
  (let ((tags (if (listp tags) tags (list tags))))
    (mapconcat #'org-glance-tag:to-string tags ":")))

(cl-defun org-glance-capture:template (tags &optional (title ""))
  "Default `org-capture' template for a new headline with TAGS, pre-filled with TITLE.
TAGS is a tag symbol or a list of tag symbols."
  (cl-check-type title string)
  (format "* %s%%?  :%s:" title (org-glance-capture:--format-tags tags)))

(cl-defun org-glance-capture:completing-read-tag ()
  "Prompt for a tag; candidates come from the graph's live headlines.
New tags are allowed -- the graph discovers tags from captured headlines, so no
registration step is needed.  Errors on empty input."
  (cl-assert (org-glance-initialized?))
  (let ((choice (s-trim (completing-read "Tag: " (org-glance-graph:tags org-glance-graph)))))
    (when (string-empty-p choice)
      (user-error "Tag must not be empty"))
    (intern (downcase choice))))

(cl-defun org-glance-capture (tags title &key template finalize)
  "Capture a headline tagged with TAGS (a symbol or list of symbols)."
  (declare (indent 2))

  (interactive (list (org-glance-capture:completing-read-tag)
                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type title string)
  (cl-assert (org-glance-initialized?))

  (let* ((file (make-temp-file "org-glance-" nil ".org"))
         (capture-token "_")
         (template (or template (org-glance-capture:template tags title)))
         (org-capture-templates (list (list capture-token capture-token 'entry (list 'file file) template))))
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook
              `(lambda ()
                 (unwind-protect
                     (when-let ((buffer (get-file-buffer ,file)))
                       (org-glance-graph:capture org-glance-graph buffer))
                   (when-let ((buffer (get-file-buffer ,file)))
                     (kill-buffer buffer))
                   (f-delete ,file)))
              0 t)
    (org-capture nil capture-token)
    (when finalize (org-capture-finalize))))

(provide 'org-glance-capture)
