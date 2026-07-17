;;; org-glance-llm.el --- LLM session for a headline (agnostic-llm) -*- lexical-binding: t; -*-

;;; Commentary:
;; The `l' action: pick a headline and open the `agnostic-llm' menu pinned to
;; the headline's content-addressable data directory, so the CLI's per-directory
;; context accumulates there.  The `*llm:…*' session buffer is named for the
;; headline's title (see `org-glance-llm--label'), not the data dir's hash.
;;
;; A headline's session is identified by that data DIR -- a full-hash path unique
;; to the headline, kept as the session buffer's `default-directory'.  Reopening
;; the headline switches to the live buffer instead of prompting again; the title
;; label is only cosmetic, so two same-titled headlines never share a session.

;;; Code:

(require 'cl-lib)
(require 'org-glance-core)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-material)

;; `agnostic-llm-menu' is a required dependency, but loaded lazily through its
;; autoload only when the command runs, so org-glance never pulls in vterm at
;; load or byte-compile time.
(declare-function agnostic-llm-menu "agnostic-llm" (&optional root label))

(cl-defun org-glance-llm--slug (title)
  "Downcased dash-separated slug of TITLE, or nil when it has no word chars.
Non-alphanumeric runs become one dash, edges trimmed (\"Buy milk (2L)!\" ->
\"buy-milk-2l\")."
  (let ((slug (string-trim
               (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase (or title "")))
               "-+" "-+")))
    (unless (string-empty-p slug) slug)))

(cl-defun org-glance-llm--buffer-name (label)
  "The `*llm:...*' session buffer name for LABEL."
  (format "*llm:%s*" label))

(cl-defun org-glance-llm--session-buffer (dir)
  "The live agnostic-llm session buffer rooted at DIR, or nil.
DIR is a headline's content-addressable data dir -- a full-hash path unique to
the headline -- so matching a `*llm:…*' buffer's `default-directory' against it
reuses that session, independent of the (cosmetic) title label."
  (cl-find-if (lambda (buf)
                (and (string-prefix-p "*llm:" (buffer-name buf))
                     (ignore-errors
                       (file-equal-p (buffer-local-value 'default-directory buf)
                                     (file-name-as-directory dir)))))
              (buffer-list)))

(cl-defun org-glance-llm--label (metadata dir)
  "The `*llm:...*' label for METADATA's session rooted at DIR.
METADATA's title slug (`--slug'), reading nicer than DIR's content-hash leaf.
Falls back to that leaf when the title slugs to nothing, and appends it to
disambiguate when a session for a DIFFERENT headline already holds the slug."
  (let* ((tail (file-name-nondirectory (directory-file-name dir)))
         (slug (or (org-glance-llm--slug (org-glance-headline-metadata:title metadata))
                   tail))
         (buf (get-buffer (org-glance-llm--buffer-name slug))))
    (if (and buf (not (file-equal-p (buffer-local-value 'default-directory buf)
                                    (file-name-as-directory dir))))
        (format "%s-%s" slug tail)
      slug)))

;;;###autoload
(cl-defun org-glance-llm ()
  "Choose a headline and open its `agnostic-llm' session.
Prompt for a headline like `org-glance-materialize'.  If that headline's session
buffer is already live, switch to it.  Otherwise materialize the headline and,
from its blob buffer, open `agnostic-llm-menu' with its session directory and
buffer name overridden to the headline's content-addressable data dir and
title-slug label (`org-glance-llm--label') -- the menu highlights the override,
and the CLI's context accumulates in that dir alongside the editable blob."
  (interactive)
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:pick-metadata graph))
         (id (org-glance-headline-metadata:id metadata))
         (dir (org-glance-graph:headline-data-path graph id))
         (label (org-glance-llm--label metadata dir))
         (existing (org-glance-llm--session-buffer dir)))
    (if (buffer-live-p existing)
        (switch-to-buffer existing)
      (make-directory dir t)
      ;; Materialize, then launch the menu FROM the blob buffer (its
      ;; `default-directory' is the headline's data dir).
      (switch-to-buffer (org-glance-material:open graph id))
      (agnostic-llm-menu dir label))))

(provide 'org-glance-llm)
;;; org-glance-llm.el ends here
