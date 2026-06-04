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

(defcustom org-glance-use-graph-v2 nil
  "When non-nil, route interactive commands onto the v2 graph store.
Off by default during coexistence; see MIGRATION-PLAN.md Phase 2."
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
  (let* ((candidates (cl-loop for meta in (org-glance-graph-v2:headlines graph)
                              when (or (null filter) (funcall filter meta))
                              collect (cons (org-glance-material-v2:label meta) meta)))
         (choice (completing-read prompt candidates nil t)))
    (cdr (assoc choice candidates))))

;;; Materialized buffer

(defvar org-glance-material-v2-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-material-v2-mode'.")

(define-minor-mode org-glance-material-v2-mode
  "Minor mode for a v2-graph materialized headline buffer."
  :lighter " glance-v2"
  :global nil
  :group 'org-glance
  :keymap org-glance-material-v2-mode-map)

(define-key org-glance-material-v2-mode-map (kbd "C-x C-s") #'org-glance-material-v2:apply)
(define-key org-glance-material-v2-mode-map (kbd "C-c C-q") #'kill-current-buffer)

(defvar-local org-glance-material-v2--graph nil
  "Graph backing the current materialized buffer.")
(defvar-local org-glance-material-v2--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")
(defvar-local org-glance-material-v2--base-hash nil
  "Stored hash of the headline at the time it was materialized.")

(cl-defun org-glance-material-v2:open (graph id)
  "Open headline ID from GRAPH in an editable materialized buffer.
Return the buffer.  Errors if ID is unknown or tombstoned."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (let ((headline (org-glance-graph-v2:headline graph id)))
    (unless headline
      (user-error "No live headline with id %s" id))
    (let ((buffer (get-buffer-create (format "*org-glance-v2: %s*" id))))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (org-glance-headline-v2:contents headline))
        (goto-char (point-min))
        (delay-mode-hooks (org-mode))
        (setq-local org-glance-material-v2--graph graph
                    org-glance-material-v2--id id
                    org-glance-material-v2--base-hash (org-glance-headline-v2:hash headline))
        (set-buffer-modified-p nil)
        (org-glance-material-v2-mode 1))
      buffer)))

(cl-defun org-glance-material-v2:apply ()
  "Write the materialized buffer back to its graph as a new version.
Append-only: a fresh record wins.  If the stored headline changed since
materialization (hash mismatch), prompt before overwriting."
  (interactive)
  (unless org-glance-material-v2--graph
    (user-error "Not in an org-glance v2 materialized buffer"))
  (let* ((graph org-glance-material-v2--graph)
         (id org-glance-material-v2--id)
         (stored (org-glance-graph-v2:get-headline graph id))
         (headline (org-glance-headline-v2--from-string
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (equal (org-glance-headline-v2:id headline) id)
      (user-error "Do not change ORG_GLANCE_ID in a materialized buffer (expected %s)" id))
    (when (and (org-glance-headline-metadata-v2? stored)
               (not (string= (org-glance-headline-metadata-v2:hash stored)
                             org-glance-material-v2--base-hash))
               (not (yes-or-no-p "Headline changed in the graph since materialize; overwrite? ")))
      (user-error "Aborted: headline changed in the graph"))
    (org-glance-graph-v2:add graph headline)
    (setq-local org-glance-material-v2--base-hash (org-glance-headline-v2:hash headline))
    (set-buffer-modified-p nil)
    (when (called-interactively-p 'any)
      (message "org-glance: applied %s" id))
    headline))

;;; Command

(cl-defun org-glance-materialize-v2 ()
  "Choose a headline from the v2 graph and materialize it."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let* ((graph org-glance-graph-v2)
         (metadata (org-glance-material-v2:completing-read graph))
         (id (org-glance-headline-metadata-v2:id metadata)))
    (switch-to-buffer (org-glance-material-v2:open graph id))))

;;; Read commands: open / extract (operate on the stored blob, read-only)

(cl-defun org-glance-material-v2:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several.
Reconstructs the content in a temp buffer and runs `org-open-at-point' at the
chosen link, mirroring the v1 behaviour."
  (cl-check-type headline org-glance-headline-v2)
  (org-glance-headline-v2:with-contents headline
    (delay-mode-hooks (org-mode))
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
         (metadata (org-glance-material-v2:completing-read graph :prompt "Open: "))
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
         (metadata (org-glance-material-v2:completing-read graph :prompt "Extract from: "))
         (headline (org-glance-graph-v2:headline graph (org-glance-headline-metadata-v2:id metadata))))
    (org-glance-material-v2:extract headline)))

(provide 'org-glance-material-v2)
;;; org-glance-material-v2.el ends here
