;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode mindmap.

;; Copyright (C) 2018-2025 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org) (aes) (dash) (f) (transient))
;; Keywords: org-mode, graph, mindmap
;; Homepage: https://github.com/rails-to-cosmos/org-glance
;; Source: gnu, melpa, org
;; License: GPL-3+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 'ol)
(require 'org)

(require 'org-glance-ui)
(require 'org-glance-utils)
(require 'org-glance-capture-v2)
(require 'org-glance-headline-v2)
(require 'org-glance-graph-v2)
(require 'org-glance-material-v2)
(require 'org-glance-overview-v2)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(defvar org-glance-graph-v2 nil
  "Current global instance of `org-glance-graph-v2'.
Constructed lazily by `org-glance-init-v2' (also called from
`org-glance-init'); nil until the system is initialized.")

(defgroup org-glance nil "Org-mode mindmap explorer."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Initialize org-glance in DIRECTORY: bring up the graph store and offer a
one-time migration when legacy metadata is detected."
  (unless (f-exists? directory)
    (mkdir directory t))
  (org-glance-init-v2 directory)
  (org-glance-migrate-maybe directory))

(cl-defun org-glance-init-v2 (&optional (directory org-glance-directory))
  "Init global `org-glance-graph-v2' in DIRECTORY."
  (load-library "org-element.el")  ;; temp fix https://github.com/doomemacs/doomemacs/issues/7347
  (setq org-glance-graph-v2 (org-glance-graph-v2 directory)))

(cl-defun org-glance-initialized?-v2 ()
  "Return `org-glance-graph' if system is initialized, or else return `nil'."
  org-glance-graph-v2)

;; --- Phase 1: runtime migration of legacy v1 metadata into the v2 graph -------
;;
;; The org files are canonical (see MIGRATION-PLAN.md), so migration RE-SCANS the
;; sources -- it does not trust v1's possibly-stale `begin' pointers and never
;; reads the v1 positional serialization. Legacy `*.metadata.el' files are merely
;; the trigger; they are backed up, never deleted.

(cl-defun org-glance-legacy-metadata-files (&optional (directory org-glance-directory))
  "Return the list of legacy v1 `*.metadata.el' files under DIRECTORY."
  (when (f-exists? directory)
    (cl-loop for file in (directory-files-recursively directory "\\.metadata\\.el\\'")
             unless (string-match-p "/\\.org-glance/" file)
             collect file)))

(cl-defun org-glance-migrate--source-files (directory)
  "Canonical org source files under DIRECTORY, excluding the v2 store."
  (cl-loop for file in (directory-files-recursively directory "\\.org\\(_archive\\)?\\'")
           unless (string-match-p "/\\.org-glance/" file)
           collect file))

(cl-defun org-glance-migrate--overview-file? (file)
  "Non-nil if FILE is a v1 `org-glance-overview' file (a read-only clone store).
Detected by its prop-line `mode: org-glance-overview' marker."
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (goto-char (point-min))
    (re-search-forward "mode:[ \t]*org-glance-overview" nil t)))

(cl-defun org-glance-migrate (&optional (directory org-glance-directory))
  "Rebuild the v2 graph in DIRECTORY from legacy v1 content.
Scan canonical (non-overview) org files for headlines carrying ORG_GLANCE_ID,
add them to the graph preserving ids, then back up legacy `*.metadata.el' files
to `*.metadata.el.bak'.  Return the number of headlines ingested."
  (interactive)
  (let* ((graph (org-glance-graph-v2 directory))
         (sources (org-glance-migrate--source-files directory))
         (legacy (org-glance-legacy-metadata-files directory))
         (total (length sources))
         (reporter (and (> total 0)
                        (make-progress-reporter "org-glance: migrating sources... " 0 total)))
         (count 0)
         (skipped nil))
    (cl-loop for file in sources
             for i from 1
             do (unless (org-glance-migrate--overview-file? file)
                  ;; Read-only parse in a temp buffer -- never `find-file' the
                  ;; source. `delay-mode-hooks' suppresses
                  ;; `after-change-major-mode-hook', which is what
                  ;; `global-undo-tree-mode' (and other globalized minor modes)
                  ;; hook into; combined with the temp buffer, undo-tree never
                  ;; activates, no undo is recorded and no `.~undo-tree~' files are
                  ;; written. Also skips per-file `org-mode-hook'.
                  ;;
                  ;; Per-file `condition-case' so one unreadable/mis-decoded file
                  ;; (e.g. autodetect mis-guesses a non-utf-8 encoding) is logged
                  ;; and skipped instead of aborting the whole batch.
                  (condition-case err
                      (with-temp-buffer
                        (insert-file-contents file)
                        (org-glance--org-mode)
                        (dolist (headline (org-glance-graph-v2:capture-buffer (current-buffer)))
                          (when (org-glance-headline-v2:id headline)
                            (org-glance-graph-v2:add graph headline)
                            (cl-incf count))))
                    (error
                     (push file skipped)
                     (display-warning 'org-glance
                                      (format "Skipped %s during migration: %s"
                                              file (error-message-string err))
                                      :warning))))
             (when reporter (progress-reporter-update reporter i)))
    (when reporter (progress-reporter-done reporter))
    (dolist (file legacy)
      (rename-file file (concat file ".bak") t))
    (when (called-interactively-p 'any)
      (message "org-glance: migrated %d headline(s)%s; backed up %d legacy file(s)."
               count
               (if skipped (format ", skipped %d file(s)" (length skipped)) "")
               (length legacy)))
    count))

(cl-defun org-glance-reindex (&optional (directory org-glance-directory))
  "Re-derive metadata for all headlines in DIRECTORY's v2 graph from their stored
content.  Run once after upgrading to backfill newly-added projection fields
(e.g. the `linked?'/`propertized?' flags used to filter `org-glance-open-v2'
and `org-glance-extract-v2')."
  (interactive)
  (let* ((graph (org-glance-graph-v2 directory))
         (n (org-glance-graph-v2:reindex graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: re-indexed %d headline(s)." n))
    n))

(cl-defun org-glance-graph-compact (&optional (directory org-glance-directory))
  "Compact DIRECTORY's v2 metadata store: merge sealed segments into one, drop
superseded records and tombstones, and reclaim the content of deleted headlines.
Safe to run anytime; a no-op on an already-compact store."
  (interactive)
  (let* ((graph (org-glance-graph-v2 directory))
         (n (org-glance-graph-v2:compact graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: compacted; %d live headline(s)." n))
    n))

(defvar org-glance-migrate--postponed nil
  "Non-nil after the user declined the legacy-metadata migration prompt.
Suppresses re-prompting for the rest of the session; `M-x org-glance-migrate'
remains available at any time.")

(cl-defun org-glance-migrate-maybe (&optional (directory org-glance-directory))
  "If legacy v1 metadata is present in DIRECTORY, offer to migrate it.
Ask at most once per session; declining leaves a quiet `message' hint, not a
warning.  Return non-nil if a migration was performed."
  (when (and (not org-glance-migrate--postponed)
             (org-glance-legacy-metadata-files directory))
    (if (yes-or-no-p "org-glance: legacy .metadata.el detected; migrate to the v2 graph store now? ")
        (progn (org-glance-migrate directory) t)
      (setq org-glance-migrate--postponed t)
      (message "org-glance: keeping legacy metadata for now; run `M-x org-glance-migrate' anytime to convert.")
      nil)))

(cl-defun org-glance:insert-pin-block ()
  (interactive)
  (insert "#+begin_pin" "\n\n" "#+end_pin")
  (forward-line -1))

(defface org-glance-link-materialize-face
  '((((background dark)) (:inherit default :underline "MediumPurple3"))
    (t (:inherit default :underline "Magenta")))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-overview-face
  '((((background dark)) (:inherit default :slant italic))
    (t (:inherit default :slant italic)))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(cl-defun org-glance-link:choose-thing-for-materialization ()
  (unless (org-glance-initialized?-v2)
    (user-error "org-glance: not initialized"))
  (concat "org-glance-visit:"
          (org-glance-headline-metadata-v2:id
           (org-glance-material-v2:completing-read org-glance-graph-v2))))

(cl-defun org-glance-link:choose-thing-for-opening ()
  (unless (org-glance-initialized?-v2)
    (user-error "org-glance: not initialized"))
  (concat "org-glance-open:"
          (org-glance-headline-metadata-v2:id
           (org-glance-material-v2:completing-read
            org-glance-graph-v2
            :prompt "Open: "
            :filter (lambda (m) (and (org-glance-headline-metadata-v2:active? m)
                                (org-glance-headline-metadata-v2:linked? m)))))))


(org-link-set-parameters
 "org-glance-visit"
 :follow #'org-glance-link:materialize
 :face 'org-glance-link-materialize-face
 :complete 'org-glance-link:choose-thing-for-materialization)

(org-link-set-parameters
 "org-glance-open"
 :follow #'org-glance-link:open
 :complete 'org-glance-link:choose-thing-for-opening)

(org-link-set-parameters
 "org-glance-overview"
 :follow #'org-glance-link:overview
 :face 'org-glance-link-overview-face)

(defun org-glance-link:materialize (id &optional _)
  "Materialize org-glance headline identified by ID."
  (unless (org-glance-initialized?-v2)
    (user-error "org-glance: not initialized"))
  (switch-to-buffer (org-glance-material-v2:open org-glance-graph-v2 id)))

(defun org-glance-link:open (id &optional _)
  "Open a link inside the org-glance headline identified by ID."
  (unless (org-glance-initialized?-v2)
    (user-error "org-glance: not initialized"))
  (let ((headline (org-glance-graph-v2:headline org-glance-graph-v2 id)))
    (unless headline (user-error "org-glance: headline %s not found" id))
    (org-glance-material-v2:open-link headline)))

(defun org-glance-link:overview (tag &optional _)
  "Open the overview filtered by TAG."
  (org-glance-overview-v2 (downcase tag)))

(provide 'org-glance)
;;; org-glance.el ends here
