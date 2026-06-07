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

(cl-defun org-glance-capture:template (tag &optional (title ""))
  "Default `org-capture' template for a new TAG headline, pre-filled with TITLE."
  (cl-check-type title string)
  (format "* %s%%?  :%s:" title (org-glance-tag:to-string tag)))

(cl-defun org-glance-capture:completing-read-tag ()
  "Prompt for a tag; candidates come from the graph's live headlines.
New tags are allowed -- the graph discovers tags from captured headlines, so no
registration step is needed.  Errors on empty input."
  (cl-assert (org-glance-initialized?))
  (let ((choice (s-trim (completing-read "Tag: " (org-glance-graph:tags org-glance-graph)))))
    (when (string-empty-p choice)
      (user-error "Tag must not be empty"))
    (intern (downcase choice))))

(cl-defun org-glance-capture (tag title &key template finalize)
  (declare (indent 2))

  (interactive (list (org-glance-capture:completing-read-tag)
                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type tag symbol)
  (cl-check-type title string)
  (cl-assert (org-glance-initialized?))

  (let* ((file (make-temp-file "org-glance-" nil ".org"))
         (capture-token "_")
         (template (or template (org-glance-capture:template tag title)))
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
