;; -*- lexical-binding: t -*-

(require 'cl-macs)

(require 'org-glance-headline-v2)
(require 'org-glance-graph-v2)

(cl-defun org-glance-capture-v2 (tag title &key (callback nil) (template nil))
  (declare (indent 2))

  (interactive (list (let ((tag (org-glance-tags:completing-read "Tag: " nil)))
                       (org-glance:create-tag tag))
                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type tag symbol)
  (cl-check-type title string)
  (cl-assert (org-glance-initialized?-v2))

  ;; (let ((id (org-glance-tag:generate-id tag))
  ;;       (file (make-temp-file "org-glance-" nil ".org")))

  ;;   (cl-flet ((prepare-finalize-hook ()
  ;;               (org-glance-headline-v2--from-string (buffer-string)) ;; should create graph of headlines in general
  ;;               ;; (goto-char (point-min))
  ;;               ;; (or (org-at-heading-p) (org-next-visible-heading 0))
  ;;               (org-set-property "ORG_GLANCE_ID" id)

  ;;               (org-toggle-tag (format "%s" tag) t))

  ;;             (after-finalize-hook ()
  ;;               (when-let (headline (org-glance-headline:search-buffer-by-id id))
  ;;                 (let* ((title (org-glance-headline:plain-title headline))
  ;;                        (tag-file (org-glance:tag-file-name tag))
  ;;                        (refile-dir (org-glance-headline:make-directory tag-file title))
  ;;                        (tmp-file (org-glance-headline:file-name headline))
  ;;                        (new-file (org-glance--make-file-directory (f-join refile-dir (format "%s.org" tag)))))
  ;;                   ;; (org-set-property "DIR" (abbreviate-file-name refile-dir))
  ;;                   ;; (save-buffer)
  ;;                   ;; (kill-buffer)

  ;;                   (f-move tmp-file new-file)
  ;;                   (org-glance-headline:update headline
  ;;                     :file new-file
  ;;                     :dir (abbreviate-file-name refile-dir))
  ;;                   (org-glance-overview tag)
  ;;                   (org-glance-overview:register-headline-in-metadata headline tag)
  ;;                   (org-glance-overview:register-headline-in-overview headline tag)
  ;;                   ;; (org-overview)
  ;;                   ;; (org-glance-headline:search-buffer-by-id id)
  ;;                   )))
  ;;             )
  ;;     )
  ;;   (find-file file)

  ;;   (add-hook 'org-capture-prepare-finalize-hook (lambda () )
  ;;             0 t)

  ;;   (add-hook 'org-capture-after-finalize-hook (lambda () (org-glance-capture:after-finalize-hook id tag)) 0 t)
  ;;   (when callback (add-hook 'org-capture-after-finalize-hook callback 1 t))

  ;;   (let ((org-capture-templates (list (list "_" "_" 'entry (list 'file file) template))))
  ;;     (org-capture nil "_")
  ;;     (when finalize (org-capture-finalize)))

  ;;   id)

  )

(provide 'org-glance-capture)
