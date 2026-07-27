;; -*- lexical-binding: t -*-

(require 'files)
(require 'cl-lib)
(require 'org-capture)
(require 's)

(require 'org-glance-tag)
(require 'org-glance-tag-config)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-utils)

(require 'org-glance-core)

(declare-function org-glance-material:insert-reference "org-glance-material")

(defvar org-glance-capture-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-capture-mode'.")

;; `@' at a word boundary references a graph headline (C-u adds a kind); at a
;; heading's column 0 or mid-word it self-inserts, mirroring a materialized
;; buffer's `@' (see `org-glance-material:refer').
(define-key org-glance-capture-mode-map (kbd "@") #'org-glance-capture:refer)

(define-minor-mode org-glance-capture-mode
  "Minor mode for an org-glance capture buffer.
Enabled by `org-glance-capture' in the capture buffer only, so `@' references
a graph headline there without leaking the binding into other org buffers."
  :lighter " glance-capture"
  :keymap org-glance-capture-mode-map)

(cl-defun org-glance-capture:refer (&optional arg)
  "Insert a reference to another graph headline at point, or self-insert `@'.
Run `org-glance-material:insert-reference' (which see, for the word-boundary
rules) on the global `org-glance-graph'; the captured headline has no id until
ingest, so nothing is excluded.  With ARG (`C-u @') also prompt for a kind."
  (interactive "P")
  (require 'org-glance-material)
  (org-glance-material:insert-reference org-glance-graph nil :with-kind arg))

(cl-defun org-glance-capture--format-tags (tags)
  "Format TAGS (a symbol or list of symbols) as an org tag string `:a:b:'."
  (let ((tags (org-glance-tag:as-list tags)))
    (mapconcat #'org-glance-tag:to-string tags ":")))

(cl-defun org-glance-capture:template (tags &optional (title ""))
  "`org-capture' template for a new headline with TAGS, pre-filled with TITLE.
TAGS is a tag symbol or a list of tag symbols.  When TAGS is a SINGLE tag with a
configuration (see `org-glance-tag-config'), the template is rendered from that
config's skeleton, and the config's emittable pragmas
\(`org-glance-tag-config:preamble' -- the `#+TODO:' cycle today) are prepended,
so the capture buffer cycles the tag states.
Otherwise the default `* TITLE%?  :tags:' is used, so an unconfigured tag is
byte-identical to before.  Multi-tag composition is deferred to Phase 2."
  (cl-check-type title string)
  (let* ((tags (org-glance-tag:as-list tags))
         (config (when (= 1 (length tags))
                   (org-glance-tag-config:resolve org-glance-graph (car tags)))))
    (if config
        (let ((body (org-glance-tag-config:render config title tags))
              (preamble (org-glance-tag-config:preamble config)))
          ;; A skeleton that already carries a pragma keeps its own (the entry
          ;; is the user's text); otherwise the config's pragmas lead.
          (if (and preamble (not (s-contains? "#+TODO:" body)))
              (concat preamble body)
            body))
      (format "* %s%%?  :%s:" title (org-glance-capture--format-tags tags)))))

(cl-defun org-glance-capture:completing-read-tag ()
  "Prompt for a tag; candidates come from the graph's live headlines.
New tags are allowed -- the graph discovers tags from captured headlines, so no
registration step is needed.  Errors on empty input."
  (org-glance-ensure-init)
  (let ((choice (s-trim (completing-read "Tag: " (org-glance-graph:tags org-glance-graph)))))
    (when (string-empty-p choice)
      (user-error "Tag must not be empty"))
    (org-glance-tag:from-string (org-glance-tag:validate-string choice))))

(cl-defun org-glance-capture--split-preamble (template)
  "Split TEMPLATE into (PREAMBLE . ENTRY).
PREAMBLE is file-level keywords (`#+TODO:' etc.) that precede the first heading;
ENTRY is the org entry starting at the first `*'.  org-capture type `entry'
requires a valid heading, so file keywords must be written to the target file
separately."
  (if (string-prefix-p "*" template)
      (cons nil template)
    (let ((lines (s-lines template)))
      (cl-loop for tail on lines
               when (string-prefix-p "*" (car tail))
               return (cons (s-join "\n" (cl-subseq lines 0 (- (length lines) (length tail))))
                            (s-join "\n" tail))
               finally return (cons template nil)))))

;;;###autoload
(cl-defun org-glance-capture (tags title &key template finalize)
  "Capture a headline tagged with TAGS (a symbol or list of symbols)."
  (declare (indent 2))

  (interactive (list (org-glance-capture:completing-read-tag)
                     (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t ""))))

  (cl-check-type title string)
  (mapc #'org-glance-tag:validate-string (org-glance-tag:as-list tags))
  (org-glance-ensure-init)

  (let* ((file (make-temp-file "org-glance-" nil ".org"))
         (capture-token "_")
         (full-template (or template (org-glance-capture:template tags title)))
         (split (org-glance-capture--split-preamble full-template))
         (preamble (car split))
         (entry (cdr split))
         (org-capture-templates (list (list capture-token capture-token 'entry (list 'file file) entry))))
    (when preamble
      (f-write-text (concat preamble "\n") 'utf-8 file))
    (find-file file)
    ;; The temp buffer is discarded once its content is in the graph; guard it
    ;; so any kill -- the after-finalize discard, or org-capture's own teardown
    ;; -- is silent, with no `Buffer modified; kill anyway?' confirmation.
    (add-hook 'kill-buffer-query-functions #'org-glance--kill-buffer-noconfirm nil t)
    (add-hook 'org-capture-after-finalize-hook
              `(lambda ()
                 (unwind-protect
                     (when-let ((buffer (get-file-buffer ,file)))
                       (org-glance-graph:capture org-glance-graph buffer))
                   (org-glance--discard-buffer (get-file-buffer ,file))
                   (f-delete ,file)))
              0 t)
    (org-capture nil capture-token)
    ;; org-capture leaves its buffer current; enable `@' references there.
    (org-glance-capture-mode 1)
    (when finalize (org-capture-finalize))))

(provide 'org-glance-capture)
