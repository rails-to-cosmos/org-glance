;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-macs)

(require 'org-glance-headline-v2)
(require 'org-glance-graph-v2)

(cl-defun org-glance-capture-v2 (tag title &key (template nil))
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
         (temp-capture-token "_")
         (org-capture-templates (list (list temp-capture-token temp-capture-token 'entry (list 'file file) template))))
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook
              #'(lambda () (unwind-protect
                          (org-glance-graph-v2:capture-buffer)
                        (kill-buffer (get-file-buffer file))
                        (f-delete file)))
              0 t)
    (org-capture nil temp-capture-token)
    ;; (when finalize (org-capture-finalize))
    ))

(provide 'org-glance-capture-v2)
