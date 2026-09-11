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

(define-key org-glance-capture-mode-map (kbd "@") #'org-glance-capture:refer)

(define-minor-mode org-glance-capture-mode
  "Minor mode for an org-glance capture buffer, with `@' for references.
`org-glance-capture' enables it in its capture buffer only."
  :lighter " glance-capture"
  :keymap org-glance-capture-mode-map)

(cl-defun org-glance-capture:refer (&optional arg)
  "Insert a reference to another graph headline at point, or self-insert `@'.
See `org-glance-material:insert-reference'; ARG (`C-u @') also reads a kind."
  (interactive "P")
  (require 'org-glance-material)
  (org-glance-material:insert-reference org-glance-graph nil :with-kind arg))

(cl-defun org-glance-capture:template (tags &optional (title ""))
  "Return the `org-capture' template for a headline with TAGS, titled TITLE.
TAGS is a tag symbol or a list of them.  A single configured tag renders its
config's skeleton prefixed with its pragmas; others get `* TITLE%?  :tags:'."
  (cl-check-type title string)
  (let* ((tags (org-glance-tag:as-list tags))
         (config (when (= 1 (length tags))
                   (org-glance-tag-config:resolve org-glance-graph (car tags)))))
    (if config
        (let ((body (org-glance-tag-config:render config title tags))
              (preamble (org-glance-tag-config:preamble config)))
          (if (and preamble (not (s-contains? "#+TODO:" body)))
              (concat preamble body)
            body))
      (format "* %s%%?  :%s:" title
              (mapconcat #'org-glance-tag:to-string tags ":")))))

(cl-defun org-glance-capture:completing-read-tag ()
  "Prompt for a tag among the graph's live tags; a new tag is allowed.
Signal `user-error' on empty input."
  (org-glance-ensure-init)
  (let ((choice (s-trim (completing-read "Tag: " (org-glance-graph:tags org-glance-graph)))))
    (when (string-empty-p choice)
      (user-error "Tag must not be empty"))
    (org-glance-tag:from-string (org-glance-tag:validate-string choice))))

(cl-defun org-glance-capture--split-preamble (template)
  "Split TEMPLATE into (PREAMBLE . ENTRY) at its first heading.
PREAMBLE holds the file keywords, which an `entry' capture cannot take."
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
  "Capture a headline titled TITLE, tagged with TAGS (a symbol or list of them).
TEMPLATE overrides `org-glance-capture:template'; FINALIZE finishes at once."
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
    (add-hook 'kill-buffer-query-functions #'org-glance--kill-buffer-noconfirm nil t)
    (add-hook 'org-capture-after-finalize-hook
              (lambda ()
                (unwind-protect
                    (when-let* ((buffer (get-file-buffer file)))
                      (org-glance-graph:capture org-glance-graph buffer))
                  (org-glance--discard-buffer (get-file-buffer file))
                  (f-delete file)))
              0 t)
    (org-capture nil capture-token)
    ;; org-capture leaves its buffer current; enable `@' references there.
    (org-glance-capture-mode 1)
    (when finalize (org-capture-finalize))))

(provide 'org-glance-capture)
