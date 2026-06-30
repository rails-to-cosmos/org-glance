;; -*- lexical-binding: t -*-

;;; org-glance-material.el --- graph-backed selection + materialize/sync

;;; Commentary:
;; Command layer over the graph.  Selection lists live headlines from the
;; graph; materialize opens a headline's content blob in an editable buffer;
;; apply writes it back as a NEW version (the store is append-only,
;; last-write-wins), guarded by a hash check against concurrent changes.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'org)
(require 'ol)
(require 's)

(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-tag-config)
(require 'org-glance-datetime-mode)

;; Defined in org-glance.el (which requires this file); referenced only at runtime.
(defvar org-glance-graph)
(declare-function org-glance-initialized? "org-glance")

;;; Selection

(cl-defun org-glance-material:label (metadata)
  "Completing-read label for headline METADATA: \"[tags] title\"."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (append (org-glance-headline-metadata:tags metadata) nil)))
    (concat (if tags (format "[%s] " (s-join "," tags)) "")
            (org-glance--title-clean (org-glance-headline-metadata:title metadata)))))

(cl-defun org-glance-material:completing-read (graph &key (prompt "Headline: ") filter)
  "Choose a live headline from GRAPH and return its metadata.
FILTER, if non-nil, is a predicate on the metadata."
  (cl-check-type graph org-glance-graph)
  ;; FILTER often calls `active?'/`done?', which read the buffer-local
  ;; `org-done-keywords' -- nil in this command/minibuffer context.  Bind the
  ;; user's done set so the filter is correct regardless of the current buffer.
  (let* ((org-done-keywords (org-glance--done-keywords))
         (candidates (cl-loop for meta in (org-glance-graph:headlines graph)
                              when (or (null filter) (funcall filter meta))
                              collect (cons (org-glance-material:label meta) meta))))
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

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-material-mode'.")

(define-minor-mode org-glance-material-mode
  "Minor mode for a materialized headline buffer."
  :lighter " glance"
  :global nil
  :group 'org-glance
  :keymap org-glance-material-mode-map
  (when org-glance-material-mode
    ;; org requires tab-width 8; no tabs.
    (setq tab-width 8 indent-tabs-mode nil)
    ;; Advance only the earliest of multiple repeatable timestamps on repeat.
    (org-glance-datetime-mode 1)))

(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)

(defvar-local org-glance-material--graph nil
  "Graph backing the current materialized buffer.")
(defvar-local org-glance-material--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")
(defvar-local org-glance-material--cycle nil
  "Per-tag `#+TODO:'-style cycle string for this buffer's headline, or nil.
Used by `org-glance-material:sync' to re-parse the saved buffer with the tag's
own keywords in scope (so a state like READING is not folded into the title).")

(defvar org-glance-material:sync-functions nil
  "Abnormal hook run after a materialized save refreshed the metadata index.
Each function is called with GRAPH and the fresh METADATA record; views (e.g.
open overview buffers) subscribe here to stay coherent with the store.")

(cl-defun org-glance-material:sync ()
  "Refresh the graph metadata index from the just-saved materialized blob.
Buffer-local `after-save-hook': the file save already persisted the content, so
this appends a fresh metadata record (append-only) and announces it via
`org-glance-material:sync-functions'.  No-op if the buffer's ORG_GLANCE_ID
was changed."
  (when (and org-glance-material--graph org-glance-material--id)
    (let* ((graph org-glance-material--graph)
           (id org-glance-material--id)
           ;; Re-parse with the tag's todo cycle GLOBALLY bound, so it reaches
           ;; `--from-string's internal temp buffer and a state like READING is
           ;; recognised instead of folding into the title.  `org-todo-keywords' is
           ;; not buffer-local here (see `material:open'), so this `let' binds the
           ;; global value the temp buffer's `org-mode' reads.
           (headline (let ((org-todo-keywords
                            (if org-glance-material--cycle
                                (list (cons 'sequence (split-string org-glance-material--cycle)))
                              org-todo-keywords)))
                       (org-glance-headline--from-string
                        (buffer-substring-no-properties (point-min) (point-max))))))
      (if (equal (org-glance-headline:id headline) id)
          (let ((metadata (org-glance-headline:metadata headline)))
            (org-glance-graph:insert graph (list metadata))
            (run-hook-with-args 'org-glance-material:sync-functions graph metadata))
        (message "org-glance: ORG_GLANCE_ID changed (expected %s); metadata not updated" id)))))

;;; Repeated headlines: clone-on-repeat
;;
;; When a repeated headline is completed, optionally preserve the finished
;; repetition as a NEW graph headline (fresh id, repeater disarmed) before org
;; bumps the timestamps, then trim the live headline back to its header and
;; pinned blocks -- the accumulated history lives on in the clone.

(defcustom org-glance-clone-on-repeat-p nil
  "Create a new headline copy when repeating rather than modifying in place."
  :group 'org-glance
  :type 'boolean)

(cl-defun org-glance-material:clone-on-repeat (&rest _)
  "Preserve the completed repetition of the materialized headline.
Runs `:before' `org-auto-repeat-maybe' (the headline is still in its done
state): snapshot the subtree, disarm its repeaters, strip its ORG_GLANCE_ID and
capture it into the graph as a new headline."
  (when (and org-glance-clone-on-repeat-p
             org-glance-material-mode
             org-glance-material--graph
             (member (org-get-todo-state) org-done-keywords)
             (org-glance-datetime-headline-repeated-p))
    (let ((graph org-glance-material--graph)
          (contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert contents)
        (org-glance--org-mode)
        (goto-char (point-min))
        (org-glance-datetime-reset-buffer-timestamps-except-earliest)
        (goto-char (point-min))
        (org-delete-property "ORG_GLANCE_ID") ;; the clone gets a fresh id
        (org-glance-graph:capture graph (current-buffer))))))

(cl-defun org-glance-material:cleanup-after-repeat (&rest _)
  "Trim the repeated materialized headline to its header and pinned blocks.
Runs `:after' `org-auto-repeat-maybe'; only meaningful when
`org-glance-material:clone-on-repeat' preserved the previous repetition."
  (when (and org-glance-clone-on-repeat-p
             org-glance-material-mode
             (org-glance-datetime-headline-repeated-p))
    (save-excursion
      (goto-char (point-min))
      (let ((header (s-trim (buffer-substring-no-properties
                             (point)
                             (save-excursion (org-end-of-meta-data) (point)))))
            (pinned (cl-loop while (search-forward "#+begin_pin" nil t)
                             collect (save-excursion
                                       (beginning-of-line)
                                       (buffer-substring-no-properties
                                        (point)
                                        (progn (search-forward "#+end_pin" nil t)
                                               (point)))))))
        (delete-region (point-min) (point-max))
        (insert (s-join "\n\n" (cons header pinned)) "\n")
        (goto-char (point-min))
        (org-delete-property "LAST_REPEAT")))))

;; Idempotent on reload: `advice-add' is a no-op for an already-added function.
(advice-add 'org-auto-repeat-maybe :before #'org-glance-material:clone-on-repeat '((depth . -90)))
(advice-add 'org-auto-repeat-maybe :after #'org-glance-material:cleanup-after-repeat)

(cl-defun org-glance-material:open (graph id)
  "Open headline ID from GRAPH for editing, as its content-blob file.
Return the buffer.  Errors if ID is unknown, tombstoned, or has no stored blob."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:get-headline graph id)))
    (unless (org-glance-headline-metadata? meta)
      (user-error "No live headline with id %s" id))
    (let ((path (f-join (org-glance-graph:headline-data-path graph id) "data.org"))
          (cycle (org-glance-tag-config:cycle-for-filter
                  graph (list :tags (append (org-glance-headline-metadata:tags meta) nil)))))
      (unless (f-exists? path)
        (user-error "No stored content for id %s" id))
      (let ((buffer (find-file-noselect path)))
        (with-current-buffer buffer
          (rename-buffer (format "*org-glance: %s*" (org-glance-headline-metadata:title meta)) t)
          (setq-local org-glance-material--graph graph
                      org-glance-material--id id)
          ;; Save atomically: write a temp file then rename it over data.org, so a
          ;; crash mid-save can never truncate the blob (the canonical materialized
          ;; file).  Mirrors the temp-then-rename in `org-glance-graph:put-content'.
          (setq-local file-precious-flag t)
          ;; A configured tag's todo cycle is NOT stored in the blob (kept clean);
          ;; stash it so `org-glance-material:sync' can re-parse the save with the
          ;; tag's own keywords in scope (else a state like READING folds into the
          ;; title).  NB do NOT `setq-local' `org-todo-keywords' here -- that would
          ;; make it buffer-local, and sync's dynamic `let' must bind it GLOBALLY to
          ;; reach `--from-string's internal temp buffer.
          (setq-local org-glance-material--cycle cycle)
          (add-hook 'after-save-hook #'org-glance-material:sync nil t)
          (org-glance-material-mode 1))
        buffer))))

(cl-defun org-glance-material:apply ()
  "Save the materialized buffer: write the blob and sync its metadata."
  (interactive)
  (unless org-glance-material--graph
    (user-error "Not in an org-glance materialized buffer"))
  (save-buffer))

;;; Commands
;;
;; The picker commands -- materialize / open / extract -- gate their candidate
;; list by the ambient `org-glance-filter-spec' (default: active headlines),
;; composed with each command's own intrinsic constraint (open needs a link,
;; extract needs a property/encryption).  The same filter is overlaid onto the
;; overview and agenda (see `org-glance-overview'); capture creates a new
;; headline, so a state filter does not apply there.

(cl-defun org-glance-materialize ()
  "Choose a headline from the graph and materialize it."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:completing-read
                    graph :filter (org-glance-filter:predicate org-glance-filter-spec)))
         (id (org-glance-headline-metadata:id metadata)))
    (switch-to-buffer (org-glance-material:open graph id))))

;;; Read commands: open / extract (operate on the stored blob, read-only)

(cl-defun org-glance-material:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several.
Reconstructs the content in a temp buffer and runs `org-open-at-point' at the
chosen link, mirroring the v1 behaviour."
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
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

(cl-defun org-glance-open ()
  "Choose a headline from the graph and open a link inside it."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (let* ((graph org-glance-graph)
         (keep? (org-glance-filter:predicate org-glance-filter-spec))
         (metadata (org-glance-material:completing-read
                    graph :prompt "Open: "
                    :filter (lambda (m) (and (funcall keep? m)
                                        (org-glance-headline-metadata:linked? m)))))
         (headline (org-glance-graph:headline graph (org-glance-headline-metadata:id metadata))))
    (org-glance-material:open-link headline)))

(cl-defun org-glance-material:extract (headline &optional key)
  "Copy a key-value pair from HEADLINE's contents to the kill ring; return value.
With KEY, extract it non-interactively; otherwise prompt."
  (cl-check-type headline org-glance-headline)
  (let ((pairs (org-glance-headline:properties headline)))
    (unless pairs (user-error "No key-value pairs in headline"))
    (let* ((key (or key (completing-read "Extract: " pairs nil t)))
           (value (alist-get key pairs nil nil #'string=)))
      (kill-new value)
      (when (called-interactively-p 'any) (message "Copied: %s" value))
      value)))

(cl-defun org-glance-extract ()
  "Choose a headline from the graph and extract a key-value pair from it."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (let* ((graph org-glance-graph)
         (keep? (org-glance-filter:predicate org-glance-filter-spec))
         (metadata (org-glance-material:completing-read
                    graph :prompt "Extract from: "
                    :filter (lambda (m) (and (funcall keep? m)
                                        (or (org-glance-headline-metadata:propertized? m)
                                            (org-glance-headline-metadata:encrypted? m))))))
         (headline (org-glance-graph:headline graph (org-glance-headline-metadata:id metadata))))
    (org-glance-material:extract headline)))

(provide 'org-glance-material)
;;; org-glance-material.el ends here
