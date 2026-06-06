;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-macs)
(require 'org-capture)

(require 'org-glance-tag)
(require 'org-glance-headline-v2)
(require 'org-glance-graph-v2)

;; Defined in org-glance.el, which requires this file (cycle) -- runtime-only refs.
(declare-function org-glance-tags:completing-read "org-glance")
(declare-function org-glance:create-tag "org-glance")
(declare-function org-glance-initialized?-v2 "org-glance")

(cl-defun org-glance-capture-v2:template (tag &optional (title ""))
  "Default `org-capture' template for a new TAG headline, pre-filled with TITLE."
  (cl-check-type title string)
  (format "* %s%%?  :%s:" title (org-glance-tag:to-string tag)))

(cl-defun org-glance-capture-v2 (tag title &key template finalize)
  (declare (indent 2))

  (interactive (list (let ((tag (org-glance-tags:completing-read "Tag: " nil)))
                       (org-glance:create-tag tag)
                       tag)

                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type tag symbol)
  (cl-check-type title string)
  (cl-assert (org-glance-initialized?-v2))

  (let* ((file (make-temp-file "org-glance-" nil ".org"))
         (capture-token "_")
         (template (or template (org-glance-capture-v2:template tag title)))
         (org-capture-templates (list (list capture-token capture-token 'entry (list 'file file) template))))
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook
              `(lambda ()
                 (unwind-protect
                     (when-let ((buffer (get-file-buffer ,file)))
                       (org-glance-graph-v2:capture org-glance-graph-v2 buffer))
                   (when-let ((buffer (get-file-buffer ,file)))
                     (kill-buffer buffer))
                   (f-delete ,file)))
              0 t)
    (org-capture nil capture-token)
    (when finalize (org-capture-finalize))))

(provide 'org-glance-capture-v2)
