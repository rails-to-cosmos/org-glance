;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-macs)
(require 'org-capture)
(require 's)

(require 'org-glance-tag)
(require 'org-glance-tag-config)
(require 'org-glance-headline)
(require 'org-glance-graph)

;; Defined in org-glance.el, which requires this file (cycle) -- runtime-only refs.
(declare-function org-glance-initialized? "org-glance")

(defvar org-glance-graph)

(cl-defun org-glance-capture--format-tags (tags)
  "Format TAGS (a symbol or list of symbols) as an org tag string `:a:b:'."
  (let ((tags (if (listp tags) tags (list tags))))
    (mapconcat #'org-glance-tag:to-string tags ":")))

(cl-defun org-glance-capture:template (tags &optional (title ""))
  "`org-capture' template for a new headline with TAGS, pre-filled with TITLE.
TAGS is a tag symbol or a list of tag symbols.  When TAGS is a SINGLE tag with a
configuration (see `org-glance-tag-config'), the template is rendered from that
config's skeleton, and -- if the config declares a `:TODO_KEYWORDS:' cycle -- a
`#+TODO:' file keyword is prepended so the capture buffer cycles the tag states.
Otherwise the default `* TITLE%?  :tags:' is used, so an unconfigured tag is
byte-identical to before.  Multi-tag composition is deferred to Phase 2."
  (cl-check-type title string)
  (let* ((tags (if (listp tags) tags (list tags)))
         (config (when (= 1 (length tags))
                   (org-glance-tag-config:resolve org-glance-graph (car tags)))))
    (if config
        (let ((body (org-glance-tag-config:render config title tags))
              (todo (org-glance-tag-config:todo config)))
          (if (and todo (not (s-contains? "#+TODO:" body)))
              (concat "#+TODO: " todo "\n" body)
            body))
      (format "* %s%%?  :%s:" title (org-glance-capture--format-tags tags)))))

(cl-defun org-glance-capture:completing-read-tag ()
  "Prompt for a tag; candidates come from the graph's live headlines.
New tags are allowed -- the graph discovers tags from captured headlines, so no
registration step is needed.  Errors on empty input."
  (cl-assert (org-glance-initialized?))
  (let ((choice (s-trim (completing-read "Tag: " (org-glance-graph:tags org-glance-graph)))))
    (when (string-empty-p choice)
      (user-error "Tag must not be empty"))
    (org-glance-tag:from-string choice)))

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
