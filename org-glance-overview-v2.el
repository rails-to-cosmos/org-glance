;; -*- lexical-binding: t -*-

;;; org-glance-overview-v2.el --- v2 graph-backed overview + agenda

;;; Commentary:
;; A read-only browser over the v2 graph: one org file (under the store) rendered
;; from headline *metadata* (cheap -- no per-headline content parse), one heading
;; per live headline with its state/title/tags/planning + ORG_GLANCE_ID. Because
;; it is a real org file with planning lines, it also feeds `org-agenda'. Browse
;; with `org-glance-overview-v2'; act on the headline at point (materialize / open
;; / extract). Gated by `org-glance-use-graph-v2'; see MIGRATION-PLAN.md Phase 2.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 's)
(require 'org)
(require 'org-agenda)

(require 'org-glance-graph-v2)
(require 'org-glance-material-v2)

(defvar org-glance-graph-v2)
(declare-function org-glance-initialized?-v2 "org-glance")

(defconst org-glance-overview-v2:header
  "#    -*- mode: org; mode: org-glance-overview-v2 -*-\n#+TITLE: org-glance overview\n\n"
  "Prop-line header written at the top of the v2 overview file.")

;;; Rendering (from metadata, not content)

(cl-defun org-glance-overview-v2:tag-string (metadata)
  (when-let ((tags (append (org-glance-headline-metadata-v2:tags metadata) nil)))
    (format "  :%s:" (s-join ":" (mapcar (lambda (x) (format "%s" x)) tags)))))

(cl-defun org-glance-overview-v2:render-headline (metadata)
  "Render METADATA as one org heading + planning + ORG_GLANCE_ID property."
  (let ((state (org-glance-headline-metadata-v2:state metadata))
        (schedule (org-glance-headline-metadata-v2:schedule metadata))
        (deadline (org-glance-headline-metadata-v2:deadline metadata)))
    (concat "* "
            (if (and (stringp state) (not (string-empty-p state))) (concat state " ") "")
            (org-glance-headline-metadata-v2:title metadata)
            (or (org-glance-overview-v2:tag-string metadata) "")
            "\n"
            (when (and (stringp schedule) (not (string-empty-p schedule)))
              (concat "SCHEDULED: " schedule "\n"))
            (when (and (stringp deadline) (not (string-empty-p deadline)))
              (concat "DEADLINE: " deadline "\n"))
            ":PROPERTIES:\n:ORG_GLANCE_ID: " (org-glance-headline-metadata-v2:id metadata) "\n:END:\n")))

(cl-defun org-glance-overview-v2:render (graph &optional tag)
  "Render GRAPH's live headlines (optionally only those carrying TAG) as org text."
  (cl-check-type graph org-glance-graph-v2)
  (let ((tag (and tag (format "%s" tag))))
    (concat org-glance-overview-v2:header
            (cl-loop for meta in (org-glance-graph-v2:headlines graph)
                     when (or (null tag)
                              (member tag (mapcar (lambda (x) (format "%s" x))
                                                  (append (org-glance-headline-metadata-v2:tags meta) nil))))
                     concat (org-glance-overview-v2:render-headline meta)))))

(cl-defun org-glance-overview-v2:file (graph)
  "Path to GRAPH's generated overview file (inside the hidden store)."
  (cl-check-type graph org-glance-graph-v2)
  (f-join (org-glance-graph-v2:store-path graph) "overview.org"))

(cl-defun org-glance-overview-v2:write (graph &optional tag)
  "(Re)generate GRAPH's overview file from the graph; return its path."
  (let ((file (org-glance-overview-v2:file graph)))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-overview-v2:render graph tag) 'utf-8 file)
    file))

;;; Browser

(defvar org-glance-overview-v2-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-overview-v2-mode'.")

(define-minor-mode org-glance-overview-v2-mode
  "Read-only browser over the v2 graph."
  :global nil
  :init-value nil
  :keymap org-glance-overview-v2-mode-map
  :after-hook (read-only-mode +1)
  (when org-glance-overview-v2-mode
    ;; org requires tab-width 8 to parse (`id-at-point' reads node properties).
    (setq tab-width 8 indent-tabs-mode nil)))

(define-key org-glance-overview-v2-mode-map (kbd "RET") #'org-glance-overview-v2:materialize)
(define-key org-glance-overview-v2-mode-map (kbd "m") #'org-glance-overview-v2:materialize)
(define-key org-glance-overview-v2-mode-map (kbd "o") #'org-glance-overview-v2:open)
(define-key org-glance-overview-v2-mode-map (kbd "e") #'org-glance-overview-v2:extract)
(define-key org-glance-overview-v2-mode-map (kbd "g") #'org-glance-overview-v2:refresh)
(define-key org-glance-overview-v2-mode-map (kbd "q") #'quit-window)

(defvar-local org-glance-overview-v2--tag nil
  "Tag filter the current overview buffer was generated with (nil = all).")

(cl-defun org-glance-overview-v2:id-at-point ()
  "ORG_GLANCE_ID of the headline at point, or signal a `user-error'."
  (or (save-excursion
        (org-back-to-heading t)
        (org-entry-get nil "ORG_GLANCE_ID"))
      (user-error "No headline at point")))

(cl-defun org-glance-overview-v2:materialize ()
  "Materialize the headline at point."
  (interactive)
  (switch-to-buffer (org-glance-material-v2:open org-glance-graph-v2 (org-glance-overview-v2:id-at-point))))

(cl-defun org-glance-overview-v2:open ()
  "Open a link inside the headline at point."
  (interactive)
  (org-glance-material-v2:open-link
   (org-glance-graph-v2:headline org-glance-graph-v2 (org-glance-overview-v2:id-at-point))))

(cl-defun org-glance-overview-v2:extract ()
  "Extract a key-value pair from the headline at point."
  (interactive)
  (org-glance-material-v2:extract
   (org-glance-graph-v2:headline org-glance-graph-v2 (org-glance-overview-v2:id-at-point))))

(cl-defun org-glance-overview-v2:visit (graph &optional tag)
  "Regenerate GRAPH's overview (filtered by TAG) and visit it read-only."
  (let ((file (org-glance-overview-v2:write graph tag)))
    ;; A generated read-only view: drop any stale buffer and re-open fresh.
    (when-let ((buffer (get-file-buffer file))) (kill-buffer buffer))
    (find-file file)
    (setq-local org-glance-overview-v2--tag tag)
    (org-glance-overview-v2-mode +1)
    (current-buffer)))

(cl-defun org-glance-overview-v2:refresh ()
  "Regenerate the current overview from the graph."
  (interactive)
  (org-glance-overview-v2:visit org-glance-graph-v2 org-glance-overview-v2--tag))

(cl-defun org-glance-overview-v2 (&optional tag)
  "Browse the v2 graph (optionally only headlines carrying TAG)."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (org-glance-overview-v2:visit org-glance-graph-v2 tag))

;;; Agenda

(cl-defun org-glance-agenda-v2 ()
  "Show an `org-agenda' over the v2 graph's scheduled/deadline headlines."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let* ((file (org-glance-overview-v2:write org-glance-graph-v2))
         (org-agenda-files (list file))
         (org-agenda-start-on-weekday nil)
         (org-agenda-overriding-header "org-glance v2 agenda"))
    (org-agenda-list nil "-7d" 21)))

(provide 'org-glance-overview-v2)
;;; org-glance-overview-v2.el ends here
