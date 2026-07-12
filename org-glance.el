;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode mindmap.

;; Copyright (C) 2018-2026 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 0.2.1
;; Package-Requires: ((emacs "29.1") (org) (aes) (dash) (f) (s) (transient) (table-view "0"))
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
(require 'dash)
(require 'f)
(require 'ol)
(require 'org)

(require 'org-glance-core)
(require 'org-glance-ui)
(require 'org-glance-utils)
(require 'org-glance-capture)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-tag-metrics)
(require 'org-glance-view)
(require 'org-glance-material)
(require 'org-glance-overview)
(require 'org-glance-table)
(require 'org-glance-tags)

;;;###autoload
(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Initialize org-glance in DIRECTORY: bring up the graph store and, when legacy
metadata is detected, warn that `M-x org-glance-migrate' can convert it."
  (load-library "org-element.el")  ;; temp fix https://github.com/doomemacs/doomemacs/issues/7347
  (unless (f-exists? directory)
    (mkdir directory t))
  (setq org-glance-graph (org-glance-graph directory))
  (org-glance-migrate-maybe directory))

;; --- Runtime migration of legacy v1 metadata into the graph -----------------
;;
;; The org files are canonical (see docs/archive/MIGRATION-PLAN.md), so migration RE-SCANS the
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
  "Canonical org source files under DIRECTORY, excluding the store."
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

;; --- Persistent migration progress ------------------------------------------
;;
;; Migration is journaled so it is idempotent and resumable: each canonical
;; source file is recorded (by its path relative to the directory) the moment it
;; is fully ingested.  A re-run -- whether after a crash mid-batch or a normal
;; second invocation -- skips every already-recorded source, so no headline is
;; ingested twice.  The journal is an append-only JSONL file at the store root
;; (one `{"source": "<relpath>"}' per line); appending after each clean file
;; keeps the record durable across an Emacs restart with at most the in-flight
;; file to redo.  Even that file cannot duplicate: a headline whose id+hash
;; already match the store is not re-added (see the ingest loop).

(cl-defun org-glance-migrate--journal-path (graph)
  "Path of GRAPH's persistent migration-progress journal."
  (f-join (org-glance-graph:store-path graph) "migration.jsonl"))

(cl-defun org-glance-migrate--migrated-sources (graph)
  "Hash-table set of source files already migrated into GRAPH's store.
Keys are paths relative to the graph directory, read from the journal; an empty
table when nothing has been migrated yet."
  (let ((path (org-glance-migrate--journal-path graph))
        (done (make-hash-table :test 'equal)))
    (when (f-exists? path)
      (dolist (line (split-string (f-read-text path 'utf-8) "\n" t))
        (ignore-errors
          (puthash (plist-get (json-parse-string line :object-type 'plist) :source)
                   t done))))
    done))

(cl-defun org-glance-migrate--record-source (graph relpath)
  "Durably append RELPATH to GRAPH's migration journal.
The file is created on first append; its directory (the store root) already
exists, having been created when the graph was constructed."
  (f-append-text (concat (json-serialize (list :source relpath)) "\n")
                 'utf-8 (org-glance-migrate--journal-path graph)))

(cl-defun org-glance-migrate--ingest-file (graph file seen)
  "Ingest ORG_GLANCE_ID-bearing headlines from source FILE into GRAPH.
SEEN is an id->content-hash table of records already in the store; a headline
whose id+hash is already present is skipped (so a re-run, or a redo of a file
interrupted mid-ingest, never appends a duplicate record), and SEEN is updated
in place as headlines are added.  Return the number of headlines actually added.

Read-only parse in a temp buffer -- never `find-file' the source.
`delay-mode-hooks' (via `org-glance--org-mode') suppresses
`after-change-major-mode-hook', which is what `global-undo-tree-mode' (and other
globalized minor modes) hook into; combined with the temp buffer, undo-tree
never activates, no undo is recorded and no `.~undo-tree~' files are written.
Also skips per-file `org-mode-hook'."
  (let ((added 0))
    (with-temp-buffer
      (insert-file-contents file)
      (org-glance--org-mode)
      (dolist (headline (org-glance-graph:capture-buffer (current-buffer)))
        (when-let ((id (org-glance-headline:id headline)))
          (let ((hash (org-glance-headline:hash headline)))
            (unless (equal (gethash id seen) hash)
              (org-glance-graph:add graph headline)
              (puthash id hash seen)
              (cl-incf added))))))
    added))

(cl-defun org-glance-migrate (&optional (directory org-glance-directory))
  "Rebuild the graph in DIRECTORY from legacy v1 content.
Scan canonical (non-overview) org files for headlines carrying ORG_GLANCE_ID and
add them to the graph preserving ids.  Idempotent and resumable: progress is
journaled per source file, so already-migrated sources are skipped and no
headline is ever ingested twice (see `org-glance-migrate--migrated-sources').
Only on a fully clean pass (no source skipped) are the legacy `*.metadata.el'
indices backed up to `*.metadata.el.bak' (never deleted); a partial or
interrupted run leaves them in place so it stays detectable and resumable.
Return the number of headlines ingested this run."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (done (org-glance-migrate--migrated-sources graph))
         (pending (cl-remove-if (lambda (f) (gethash (f-relative f directory) done))
                                (org-glance-migrate--source-files directory)))
         ;; One id->hash snapshot of the store, built once and updated as we add,
         ;; so dedup is an O(1) lookup instead of a per-headline segment scan.
         (seen (and pending
                    (let ((h (make-hash-table :test 'equal)))
                      (dolist (meta (org-glance-graph:headlines graph) h)
                        (puthash (org-glance-headline-metadata:id meta)
                                 (org-glance-headline-metadata:hash meta) h)))))
         (total (length pending))
         (reporter (and (> total 0)
                        (make-progress-reporter "org-glance: migrating sources... " 0 total)))
         (count 0)
         (skipped nil))
    (cl-loop for file in pending
             for i from 1
             for relpath = (f-relative file directory)
             ;; Per-file `condition-case' so one unreadable/mis-decoded file
             ;; (e.g. autodetect mis-guesses a non-utf-8 encoding) is logged and
             ;; skipped instead of aborting the whole batch.  A skipped file is
             ;; NOT journaled, so a later run retries it.  Overview clones are
             ;; journaled but never ingested, so a clone can't override its
             ;; source and is also skipped on resume.
             do (condition-case err
                    (progn
                      (unless (org-glance-migrate--overview-file? file)
                        (cl-incf count (org-glance-migrate--ingest-file graph file seen)))
                      (org-glance-migrate--record-source graph relpath))
                  (error
                   (push file skipped)
                   (display-warning 'org-glance
                                    (format "Skipped %s during migration: %s"
                                            file (error-message-string err))
                                    :warning)))
             (when reporter (progress-reporter-update reporter i)))
    (when reporter (progress-reporter-done reporter))
    (let ((legacy (org-glance-legacy-metadata-files directory)))
      ;; Back up legacy indices only on a clean pass.
      (unless skipped
        (dolist (file legacy)
          (rename-file file (concat file ".bak") t)))
      (when (called-interactively-p 'any)
        (message "org-glance: migrated %d headline(s)%s; %s %d legacy file(s)."
                 count
                 (if skipped (format ", skipped %d file(s)" (length skipped)) "")
                 (if skipped "kept" "backed up")
                 (length legacy))))
    count))

(cl-defun org-glance-reindex (&optional (directory org-glance-directory))
  "Re-derive metadata for all headlines in DIRECTORY's graph from their stored
content.  Run once after upgrading to backfill newly-added projection fields
(e.g. the `linked?'/`propertized?' flags used to filter `org-glance-open'
and `org-glance-extract')."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (n (org-glance-graph:reindex graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: re-indexed %d headline(s)." n))
    n))

(cl-defun org-glance-graph-compact (&optional (directory org-glance-directory))
  "Compact DIRECTORY's metadata store: merge sealed segments into one, drop
superseded records and tombstones, and reclaim the content of deleted headlines.
Safe to run anytime; a no-op on an already-compact store."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (n (org-glance-graph:compact graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: compacted; %d live headline(s)." n))
    n))

(defvar org-glance-migrate--warned nil
  "Non-nil once the legacy-metadata warning has fired this session.
Keeps `org-glance-migrate-maybe' from re-warning on a repeated init.")

(cl-defun org-glance-migrate-maybe (&optional (directory org-glance-directory))
  "If legacy v1 metadata is present in DIRECTORY, warn that it can be converted.
Never migrates automatically and never prompts -- the legacy store is left
untouched.  Emits the warning at most once per session; `M-x org-glance-migrate'
performs the conversion whenever the user chooses.  Always returns nil."
  (when (and (not org-glance-migrate--warned)
             (org-glance-legacy-metadata-files directory))
    (setq org-glance-migrate--warned t)
    (display-warning 'org-glance
                     "Legacy .metadata.el detected; run `M-x org-glance-migrate' to convert it to the graph store."
                     :warning))
  nil)

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
  (unless (org-glance-initialized?)
    (user-error "org-glance: not initialized"))
  (->> (org-glance-material:completing-read org-glance-graph)
       org-glance-headline-metadata:id
       (concat "org-glance-visit:")))

(cl-defun org-glance-link:choose-thing-for-opening ()
  (unless (org-glance-initialized?)
    (user-error "org-glance: not initialized"))
  (->> (org-glance-material:completing-read
        org-glance-graph
        :prompt "Open: "
        :filter (lambda (m) (and (org-glance-headline-metadata:active? m)
                            (org-glance-headline-metadata:linked? m))))
       org-glance-headline-metadata:id
       (concat "org-glance-open:")))

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
  (unless (org-glance-initialized?)
    (user-error "org-glance: not initialized"))
  (switch-to-buffer (org-glance-material:open org-glance-graph id)))

(defun org-glance-link:open (id &optional _)
  "Open a link inside the org-glance headline identified by ID."
  (unless (org-glance-initialized?)
    (user-error "org-glance: not initialized"))
  (let ((headline (org-glance-graph:headline org-glance-graph id)))
    (unless headline (user-error "org-glance: headline %s not found" id))
    (org-glance-material:open-link headline)))

(defun org-glance-link:overview (tag &optional _)
  "Open the overview filtered by TAG."
  (org-glance-overview (downcase tag)))

(provide 'org-glance)
;;; org-glance.el ends here
