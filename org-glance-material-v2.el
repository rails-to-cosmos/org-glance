;; -*- lexical-binding: t -*-

;;; org-glance-material-v2.el --- v2 graph-backed selection + materialize/sync

;;; Commentary:
;; Phase 2 (coexistence) command layer over the v2 graph.  Selection lists live
;; headlines from the graph; materialize opens a headline's content blob in an
;; editable buffer; apply writes it back as a NEW version (the store is
;; append-only, last-write-wins), guarded by a hash check against concurrent
;; changes.  Gated by `org-glance-use-graph-v2'; see MIGRATION-PLAN.md Phase 2.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'org)
(require 'ol)
(require 's)

(require 'org-glance-utils)
(require 'org-glance-headline-v2)
(require 'org-glance-graph-v2)

;; Defined in org-glance.el (which requires this file); referenced only at runtime.
(defvar org-glance-graph-v2)
(declare-function org-glance-initialized?-v2 "org-glance")

(defcustom org-glance-use-graph-v2 t
  "When non-nil, route interactive commands onto the v2 graph store.
Default since Phase 2: `org-glance:materialize' / `:open' / `:extract' and
`org-glance-overview' use the v2 graph for interactive (no-argument)
invocations.  Set to nil to fall back to the v1 store.  Programmatic /
link-follow calls (which pass an explicit headline or tag) always use v1.
See MIGRATION-PLAN.md Phase 2."
  :group 'org-glance
  :type 'boolean)

;;; Selection

(cl-defun org-glance-material-v2:label (metadata)
  "Completing-read label for headline METADATA: \"[tags] title\"."
  (cl-check-type metadata org-glance-headline-metadata-v2)
  (let ((tags (append (org-glance-headline-metadata-v2:tags metadata) nil)))
    (concat (if tags (format "[%s] " (s-join "," tags)) "")
            (org-glance-headline-metadata-v2:title metadata))))

(cl-defun org-glance-material-v2:completing-read (graph &key (prompt "Headline: ") filter)
  "Choose a live headline from GRAPH and return its metadata.
FILTER, if non-nil, is a predicate on the metadata."
  (cl-check-type graph org-glance-graph-v2)
  (let ((candidates (cl-loop for meta in (org-glance-graph-v2:headlines graph)
                             when (or (null filter) (funcall filter meta))
                             collect (cons (org-glance-material-v2:label meta) meta))))
    (unless candidates
      (user-error "No matching headlines (run `M-x org-glance-reindex' if you upgraded)"))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

;;; Materialized buffer
;;
;; A materialized headline is its content blob (`…/data/<id>/data.org`) opened as
;; a REAL file.  Saving is therefore an ordinary file save (works under any
;; config -- evil, super-save, whatever binds the save command), and a
;; buffer-local `after-save-hook' refreshes the graph metadata index from the
;; saved content.  Emacs's own "file changed on disk" handling covers concurrent
;; edits, so there is no custom conflict prompt.

(defvar org-glance-material-v2-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-material-v2-mode'.")

(define-minor-mode org-glance-material-v2-mode
  "Minor mode for a v2-graph materialized headline buffer."
  :lighter " glance-v2"
  :global nil
  :group 'org-glance
  :keymap org-glance-material-v2-mode-map
  (when org-glance-material-v2-mode
    ;; org requires tab-width 8; no tabs.
    (setq tab-width 8 indent-tabs-mode nil)))

(define-key org-glance-material-v2-mode-map (kbd "C-c C-q") #'kill-current-buffer)

(defvar-local org-glance-material-v2--graph nil
  "Graph backing the current materialized buffer.")
(defvar-local org-glance-material-v2--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")

(cl-defun org-glance-material-v2:sync ()
  "Refresh the graph metadata index from the just-saved materialized blob.
Buffer-local `after-save-hook': the file save already persisted the content, so
this appends a fresh metadata record (append-only).  No-op if the buffer's
ORG_GLANCE_ID was changed."
  (when (and org-glance-material-v2--graph org-glance-material-v2--id)
    (let* ((graph org-glance-material-v2--graph)
           (id org-glance-material-v2--id)
           (headline (org-glance-headline-v2--from-string
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (if (equal (org-glance-headline-v2:id headline) id)
          (org-glance-graph-v2:insert graph (list (org-glance-headline-v2:metadata headline)))
        (message "org-glance: ORG_GLANCE_ID changed (expected %s); metadata not updated" id)))))

(cl-defun org-glance-material-v2:open (graph id)
  "Open headline ID from GRAPH for editing, as its content-blob file.
Return the buffer.  Errors if ID is unknown, tombstoned, or has no stored blob."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (let ((meta (org-glance-graph-v2:get-headline graph id)))
    (unless (org-glance-headline-metadata-v2? meta)
      (user-error "No live headline with id %s" id))
    (let ((path (f-join (org-glance-graph-v2:headline-data-path graph id) "data.org")))
      (unless (f-exists? path)
        (user-error "No stored content for id %s" id))
      (let ((buffer (find-file-noselect path)))
        (with-current-buffer buffer
          (rename-buffer (format "*org-glance: %s*" (org-glance-headline-metadata-v2:title meta)) t)
          (setq-local org-glance-material-v2--graph graph
                      org-glance-material-v2--id id)
          (add-hook 'after-save-hook #'org-glance-material-v2:sync nil t)
          (org-glance-material-v2-mode 1))
        buffer))))

(cl-defun org-glance-material-v2:apply ()
  "Save the materialized buffer: write the blob and sync its metadata."
  (interactive)
  (unless org-glance-material-v2--graph
    (user-error "Not in an org-glance v2 materialized buffer"))
  (save-buffer))

;;; Command

(cl-defun org-glance-materialize-v2 ()
  "Choose a headline from the v2 graph and materialize it."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let* ((graph org-glance-graph-v2)
         (metadata (org-glance-material-v2:completing-read
                    graph :filter #'org-glance-headline-metadata-v2:active?))
         (id (org-glance-headline-metadata-v2:id metadata)))
    (switch-to-buffer (org-glance-material-v2:open graph id))))

;;; Read commands: open / extract (operate on the stored blob, read-only)

(cl-defun org-glance-material-v2:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several.
Reconstructs the content in a temp buffer and runs `org-open-at-point' at the
chosen link, mirroring the v1 behaviour."
  (cl-check-type headline org-glance-headline-v2)
  (org-glance-headline-v2:with-contents headline
    (cl-loop for (link title pos) in (org-glance--parse-links)
             unless (s-starts-with-p "[[org-glance-" link)
             collect (list title pos) into links
             finally
             (goto-char (cond ((> (length links) 1)
                               (cadr (assoc (completing-read "Open link: " links nil t) links #'string=)))
                              ((= (length links) 1) (cadar links))
                              (t (user-error "No links in headline"))))
             ;; Mirror v1: open `file:' links in the same window.
             (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
               (org-open-at-point)))))

(cl-defun org-glance-open-v2 ()
  "Choose a headline from the v2 graph and open a link inside it."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let* ((graph org-glance-graph-v2)
         (metadata (org-glance-material-v2:completing-read
                    graph :prompt "Open: "
                    :filter (lambda (m) (and (org-glance-headline-metadata-v2:active? m)
                                        (org-glance-headline-metadata-v2:linked? m)))))
         (headline (org-glance-graph-v2:headline graph (org-glance-headline-metadata-v2:id metadata))))
    (org-glance-material-v2:open-link headline)))

(cl-defun org-glance-material-v2:extract (headline &optional key)
  "Copy a key-value pair from HEADLINE's contents to the kill ring; return value.
With KEY, extract it non-interactively; otherwise prompt."
  (cl-check-type headline org-glance-headline-v2)
  (let ((pairs (org-glance-headline-v2:properties headline)))
    (unless pairs (user-error "No key-value pairs in headline"))
    (let* ((key (or key (completing-read "Extract: " pairs nil t)))
           (value (alist-get key pairs nil nil #'string=)))
      (kill-new value)
      (when (called-interactively-p 'any) (message "Copied: %s" value))
      value)))

(cl-defun org-glance-extract-v2 ()
  "Choose a headline from the v2 graph and extract a key-value pair from it."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let* ((graph org-glance-graph-v2)
         (metadata (org-glance-material-v2:completing-read
                    graph :prompt "Extract from: "
                    :filter (lambda (m) (and (org-glance-headline-metadata-v2:active? m)
                                        (or (org-glance-headline-metadata-v2:propertized? m)
                                            (org-glance-headline-metadata-v2:encrypted? m))))))
         (headline (org-glance-graph-v2:headline graph (org-glance-headline-metadata-v2:id metadata))))
    (org-glance-material-v2:extract headline)))

(provide 'org-glance-material-v2)
;;; org-glance-material-v2.el ends here
