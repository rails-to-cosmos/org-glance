;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-macs)
(require 'org-capture)

(require 'org-glance-headline)
(require 'org-glance-graph)

(declare-function org-glance:create-tag "org-glance.el")
(declare-function org-glance-tags:completing-read "org-glance.el")
(declare-function org-glance-initialized? "org-glance.el")

(cl-defun org-glance-capture (tag title &key (template nil))
  (declare (indent 2))

  (interactive (list (let ((tag (org-glance-tags:completing-read "Tag: " nil)))
                       (org-glance:create-tag tag)
                       tag)

                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type tag symbol)
  (cl-check-type title string)
  (cl-assert (org-glance-initialized?))

  (let* ((graph (org-glance-graph))
         (id (org-glance-graph:make-id graph))
         (file (make-temp-file "org-glance-" nil ".org"))
         (capture-token "_")
         (org-capture-templates (list (list capture-token capture-token 'entry (list 'file file) template))))
    (find-file file)
    (add-hook 'org-capture-prepare-finalize-hook
              (lambda ()
                (goto-char (point-min))
                (or (org-at-heading-p) (org-next-visible-heading 0))
                (org-set-property "ORG_GLANCE_ID" id)
                (org-toggle-tag (format "%s" tag) t))
              0 t)
    (add-hook 'org-capture-after-finalize-hook
              `(lambda () (unwind-protect
                         (let* ((headlines (org-glance-graph:capture-buffer))
                                (graph (org-glance-graph)))
                           (dolist (hl headlines)
                             (org-glance-graph:add graph hl)
                             (org-glance-graph:store-headline graph hl)))
                       (kill-buffer (get-file-buffer ,file))
                       (f-delete ,file)))
              0 t)
    (org-capture nil capture-token)))

(provide 'org-glance-capture)
