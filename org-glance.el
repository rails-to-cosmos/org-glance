;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode projections

;; Copyright (C) 2018-2026 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 1.42.0.0.20260923.0
;; Package-Requires: ((emacs "29.1") (org) (aes) (dash) (f) (s) (transient) (cond-let "0") (table-view "0"))
;; Keywords: org-mode, outlines, data, database, store, projections
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
;; Projections over org-mode headlines.  The org files are the source of
;; truth -- durable and addressable; everything else is a projection.
;;
;; Every captured headline gets an `:ORG_GLANCE_ID:' and its own org file
;; under `org-glance-directory'.  Tags are collections, each free to carry
;; its own todo cycle and capture template.  Tables, overviews, the property
;; index and the write-ahead log are derived: delete one and the org files
;; rebuild it.
;;
;; Edits go back into those files as ordinary org.  Another program may edit
;; them too: the `glance' browser front end writes the same bytes and names
;; each write in `meta/EXTERNAL.jsonl', which the next read folds back in.

;;; Code:

(require 'cl-lib)
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
(require 'org-glance-property-index)
(require 'org-glance-view)
(require 'org-glance-material)
(require 'org-glance-overview)
(require 'org-glance-table)
(require 'org-glance-relations)
(require 'org-glance-tags)

(defcustom org-glance-plugins nil
  "Optional org-glance plugins to load at init, as feature-name suffixes.
`org-glance-init' requires `org-glance-<name>' for each; a failing one warns
and is skipped.  A manual `require' works identically.  Available: `llm'."
  :group 'org-glance
  :type '(repeat symbol))

(cl-defun org-glance-plugin-feature (plugin)
  "Return the library feature PLUGIN names, `org-glance-<plugin>'.
The plugin naming ABI is spelled here once; every plugin lookup uses it."
  (intern (format "org-glance-%s" plugin)))

(cl-defun org-glance--read-plugin (prompt candidates empty &optional require-match)
  "Read one of CANDIDATES (symbols) under PROMPT; return a symbol.
Signal EMPTY as a `user-error' when CANDIDATES is nil, and reject empty input."
  (unless candidates (user-error "%s" empty))
  (let ((choice (completing-read prompt (mapcar #'symbol-name candidates)
                                 nil require-match)))
    (when (string-empty-p choice) (user-error "No plugin given"))
    (intern choice)))

(cl-defun org-glance--plugins-persist (plugin verb &optional note)
  "Save `org-glance-plugins' and report PLUGIN's new state as VERB, plus NOTE.
Batch skips both the save and NOTE."
  (unless noninteractive
    (customize-save-variable 'org-glance-plugins org-glance-plugins))
  (message "org-glance: plugin `%s' %s%s" plugin verb
           (if (and note (not noninteractive)) (concat " " note) "")))

(defconst org-glance-plugins-available '(llm)
  "Known org-glance plugins, offered by `org-glance-plugin-enable'.
Each ships as its own package, `org-glance-<name>'.")

;;;###autoload
(cl-defun org-glance-plugin-enable (plugin)
  "Load PLUGIN now and save it in `org-glance-plugins' (`I').
Interactively, offer the `org-glance-plugins-available' entries not yet
enabled (free input names an external plugin), or report that none remain.
Load errors are LOUD; a library absent from `load-path' is a `user-error'."
  (interactive
   (list (org-glance--read-plugin
          "Enable plugin: "
          (cl-remove-if (lambda (p) (memq p org-glance-plugins))
                        org-glance-plugins-available)
          (format "Every known plugin is already enabled: %s"
                  (mapconcat #'symbol-name org-glance-plugins-available ", ")))))
  (let ((feature (org-glance-plugin-feature plugin)))
    (unless (require feature nil 'noerror)
      (user-error "Install the `%s' package first (e.g. `M-x package-install' or `:ensure t'); it is not on `load-path'"
                  feature)))
  (add-to-list 'org-glance-plugins plugin)
  (org-glance--plugins-persist plugin "enabled" "and saved"))

;;;###autoload
(cl-defun org-glance-plugin-disable (plugin)
  "Drop PLUGIN from `org-glance-plugins' and persist the change (`U').
Prompt from the enabled plugins.  Its library, keys and transient rows stay
until restart (invariant 26); the next init skips it."
  (interactive
   (list (org-glance--read-plugin "Disable plugin: " org-glance-plugins
                                  "No plugins are enabled" t)))
  (setq org-glance-plugins (remq plugin org-glance-plugins))
  (org-glance--plugins-persist plugin "disabled" "(its code stays until restart)"))

(cl-defun org-glance--load-plugins ()
  "Require every `org-glance-plugins' entry, each error-demoted (invariant 26).
Use `condition-case': `with-demoted-errors' re-raises under `debug-on-error'."
  (dolist (plugin org-glance-plugins)
    (condition-case err
        (require (org-glance-plugin-feature plugin) nil t)
      (error (message "org-glance: plugin load failed: %S" err)))))

;;;###autoload
(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Initialize org-glance in DIRECTORY after loading `org-glance-plugins'.
Open its graph store and warn when legacy metadata awaits `org-glance-migrate'."
  (load-library "org-element.el")  ;; temp fix https://github.com/doomemacs/doomemacs/issues/7347
  (org-glance--load-plugins)
  (unless (f-exists? directory)
    (mkdir directory t))
  (setq org-glance-graph (org-glance-graph directory))
  (org-glance-migrate-maybe directory))

;; Migration RE-SCANS the org sources: v1's `begin' pointers may be stale.

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
  "Non-nil if FILE is a v1 `org-glance-overview' clone, per its prop-line."
  (with-temp-buffer
    (insert-file-contents file nil 0 256)
    (goto-char (point-min))
    (re-search-forward "mode:[ \t]*org-glance-overview" nil t)))


(cl-defun org-glance-migrate--journal-path (graph)
  "Path of GRAPH's persistent migration-progress journal."
  (f-join (org-glance-graph:store-path graph) "migration.jsonl"))

(cl-defun org-glance-migrate--migrated-sources (graph)
  "Return a hash set of GRAPH's migrated sources, keyed by relative path."
  (let ((path (org-glance-migrate--journal-path graph))
        (done (make-hash-table :test 'equal)))
    (when (f-exists? path)
      (dolist (line (split-string (f-read-text path 'utf-8) "\n" t))
        (ignore-errors
          (puthash (plist-get (json-parse-string line :object-type 'plist) :source)
                   t done))))
    done))

(cl-defun org-glance-migrate--record-source (graph relpath)
  "Durably append RELPATH to GRAPH's migration journal."
  (f-append-text (concat (json-serialize (list :source relpath)) "\n")
                 'utf-8 (org-glance-migrate--journal-path graph)))

(cl-defun org-glance-migrate--ingest-file (graph file seen)
  "Ingest ORG_GLANCE_ID-bearing headlines from source FILE into GRAPH.
SEEN maps id to content hash of stored records: a matching headline is skipped,
an added one recorded in place.  Return the number of headlines added.
Parse in a temp buffer (invariant 12), so no mode hook or undo-tree runs."
  (let ((added 0))
    (with-temp-buffer
      (insert-file-contents file)
      (org-glance--org-mode)
      (dolist (headline (org-glance-graph:capture-buffer (current-buffer)))
        (when-let* ((id (org-glance-headline:id headline)))
          (let ((hash (org-glance-headline:hash headline)))
            (unless (equal (gethash id seen) hash)
              (org-glance-graph:add graph headline)
              (puthash id hash seen)
              (cl-incf added))))))
    added))

(cl-defun org-glance-migrate (&optional (directory org-glance-directory))
  "Rebuild DIRECTORY's graph from legacy v1 content; return the count added.
Ingest the ORG_GLANCE_ID headlines of the non-overview org sources, keeping ids.
Idempotent and resumable: a journaled source is skipped.  Only a pass that
skips no source renames each legacy `*.metadata.el' to `*.metadata.el.bak'."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (done (org-glance-migrate--migrated-sources graph))
         (pending (cl-remove-if (lambda (f) (gethash (f-relative f directory) done))
                                (org-glance-migrate--source-files directory)))
         ;; One id->hash snapshot of the store: dedup is an O(1) lookup.
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
             ;; Only a skipped file goes unjournaled, so a later run retries it.
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
  "Re-derive metadata for every headline in DIRECTORY's graph from its content.
Run it after an upgrade to backfill new fields; it also drops derived caches."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (n (org-glance-graph:reindex graph)))
    (org-glance-property-index:clear graph)   ; in-session memo (+ its file)
    ;; Identities change across upgrades, so per-key dirs would accrete forever.
    (dolist (dir (list (org-glance-graph:cache-path graph)
                       (org-glance-overview:cache-path graph)))
      (ignore-errors (f-delete dir t)))
    (when (called-interactively-p 'any)
      (message "org-glance: re-indexed %d headline(s)." n))
    n))

(cl-defun org-glance-graph-compact (&optional (directory org-glance-directory))
  "Compact DIRECTORY's metadata store; a no-op when already compact.
Merge sealed segments into one, drop superseded records and tombstones, and
reclaim deleted headlines' content.  Safe to run anytime."
  (interactive)
  (let* ((graph (org-glance-graph directory))
         (n (org-glance-graph:compact graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: compacted; %d live headline(s)." n))
    n))

(defvar org-glance-migrate--warned nil
  "Non-nil once `org-glance-migrate-maybe' has warned this session.")

(cl-defun org-glance-migrate-maybe (&optional (directory org-glance-directory))
  "Warn once per session when DIRECTORY has legacy v1 metadata; return nil.
Never migrate or prompt; `M-x org-glance-migrate' converts."
  (when (and (not org-glance-migrate--warned)
             (org-glance-legacy-metadata-files directory))
    (setq org-glance-migrate--warned t)
    (display-warning 'org-glance
                     "Legacy .metadata.el detected; run `M-x org-glance-migrate' to convert it to the graph store."
                     :warning))
  nil)

(defface org-glance-link-materialize-face
  '((((background dark)) (:inherit default :underline "MediumPurple3"))
    (t (:inherit default :underline "Magenta")))
  "Face of `org-glance-material:'/`org-glance-visit:' links (follow = edit)."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-overview-face
  '((((background dark)) (:inherit default :slant italic))
    (t (:inherit default :slant italic)))
  "Face of `org-glance-overview:' links (follow = browse a filtered view)."
  :group 'org-glance
  :group 'faces)

(cl-defun org-glance-link:complete-material ()
  "Complete an `org-glance-material:ID' link by picking a headline."
  (org-glance-ensure-init)
  (org-glance--edge->link-path
   (org-glance-headline-metadata:id
    (org-glance-material:completing-read org-glance-graph))))

(cl-defun org-glance-link:complete-open ()
  "Org link completion for `org-glance-open': pick an active, linked headline."
  (org-glance-ensure-init)
  (->> (org-glance-material:completing-read
        org-glance-graph
        :prompt "Open: "
        :filter (lambda (m) (and (org-glance-headline-metadata:active? m)
                            (org-glance-headline-metadata:linked? m))))
       org-glance-headline-metadata:id
       (concat "org-glance-open:")))

(org-link-set-parameters
 org-glance-link-material-type            ; "org-glance-material" -- the canonical edge
 :follow #'org-glance-link:material
 :face 'org-glance-link-materialize-face
 :complete #'org-glance-link:complete-material)

(org-link-set-parameters
 "org-glance-visit"                       ; legacy edge type: followed, never completed
 :follow #'org-glance-link:material
 :face 'org-glance-link-materialize-face)

(org-link-set-parameters
 "org-glance-open"
 :follow #'org-glance-link:open
 :complete #'org-glance-link:complete-open)

(org-link-set-parameters
 "org-glance-overview"
 :follow #'org-glance-link:overview
 :face 'org-glance-link-overview-face)

(defun org-glance-link:material (path &optional _)
  "Materialize the headline PATH refers to; a `?kind=' suffix is ignored.
Serves both the material and the legacy visit link types."
  (org-glance-ensure-init)
  (switch-to-buffer
   (org-glance-material:open org-glance-graph (car (split-string path "[?]")))))

(defun org-glance-link:open (id &optional _)
  "Open a link inside the org-glance headline identified by ID."
  (org-glance-ensure-init)
  (let ((headline (org-glance-graph:headline org-glance-graph id)))
    (unless headline (user-error "org-glance: headline %s not found" id))
    (org-glance-material:open-link headline)))

(defun org-glance-link:overview (path &optional _)
  "Open the overview PATH describes: \"TAG[?KEY=VALUE&...]\".
A bare TAG merges the ambient `org-glance-filter-spec' as `org-glance-overview'
does; a `?'-qualified PATH applies exactly its own filter.  Either lands in
`org-glance-overview-default-view'."
  (org-glance-ensure-init)
  (let ((spec (org-glance-filter:from-link-path path)))
    (if (string-match-p "[?]" path)
        (org-glance-overview:visit-default org-glance-graph spec)
      (org-glance-overview spec))))

(provide 'org-glance)
;;; org-glance.el ends here
