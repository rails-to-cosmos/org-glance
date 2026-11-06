;;; org-glance-config-migrate.el --- Import retired settings -*- lexical-binding: t -*-

;;; Commentary:
;; Convert saved Org settings and native layouts into shared JSON.

;;; Code:

(require 'org-glance-config)
(require 'org-glance-table-config)

(defconst org-glance-config-migrate--tree-fields
  '(("GLANCE_INBOX_TAG" "inboxTag" string)
    ("GLANCE_HEADLINE_HISTORY_DEPTH" "headlineHistoryDepth" integer)
    ("GLANCE_REPOSITORY_AUTOSYNC" "repositoryAutosync" boolean)
    ("GLANCE_REPOSITORY_REMOTE" "repositoryRemote" string)
    ("GLANCE_GOOGLE_OAUTH_CLIENT_ID" "googleOAuthClientId" string)
    ("GLANCE_DEFAULT_AGENT" "defaultAgent" string)))

(defconst org-glance-config-migrate--views
  '(("GLANCE_DEFAULT_FILTER" . "default")
    ("GLANCE_AGENDA_FILTER" . "agenda")
    ("GLANCE_ARCHIVE_FILTER" . "archive")
    ("GLANCE_NOTIFICATIONS_FILTER" . "notifications")))

(defconst org-glance-config-migrate--layouts
  '(("table-views.eld" . "tableViews")
    ("table-columns.eld" . "tableColumns")
    ("table-refs.eld" . "tableRefs")))

(defun org-glance-config-migrate--cycle (tokens)
  "Encode ordered TODO TOKENS as a shared cycle."
  (let* ((separator (member "|" tokens))
         (active (if separator (butlast tokens (length separator)) tokens))
         (cycle (make-hash-table :test 'equal)))
    (puthash "active" (vconcat active) cycle)
    (puthash "inactive" (vconcat (cdr separator)) cycle)
    cycle))

(defun org-glance-config-migrate--emacs-cycles ()
  "Return global Emacs TODO cycles with explicit inactive boundaries."
  (let ((sequences (default-value 'org-todo-keywords)))
    (when (stringp (car sequences)) (setq sequences (list (cons 'sequence sequences))))
    (vconcat
     (mapcar (lambda (sequence)
               (let ((tokens (cdr sequence)))
                 (org-glance-config-migrate--cycle
                  (if (member "|" tokens) tokens
                    (append (butlast tokens) '("|") (last tokens))))))
             sequences))))

(defun org-glance-config-migrate--org (text)
  "Return TEXT's JSON workflow and parsed file keywords."
  (with-temp-buffer
    (insert text)
    (org-glance--org-mode)
    (let* ((names (append '("TODO" "SEQ_TODO" "TYP_TODO" "TITLE" "GLANCE_STORAGE"
                           "GLANCE_STATE_COLORS")
                         (mapcar #'car org-glance-config-migrate--tree-fields)
                         (mapcar #'car org-glance-config-migrate--views)))
           (keywords (org-collect-keywords names))
           (object (make-hash-table :test 'equal))
           (cycles (mapcar
                    (lambda (entry)
                      (org-glance-config-migrate--cycle (split-string (nth 2 entry))))
                    (org-glance-tag-config:todo-declarations text))))
      (when cycles (puthash "todoCycles" (vconcat cycles) object))
      (when-let* ((title (cadr (assoc "TITLE" keywords))))
        (puthash "title" title object))
      (when-let* ((storage (cadr (assoc "GLANCE_STORAGE" keywords))))
        (puthash "storage" (downcase (string-trim storage)) object))
      (when-let* ((start (org-element-map (org-element-parse-buffer) 'headline
                          (lambda (element) (org-element-property :begin element)) nil t)))
        (puthash "template" (string-trim-right
                             (buffer-substring-no-properties start (point-max))) object))
      (cons object keywords))))

(defun org-glance-config-migrate--system (text object)
  "Import system TEXT into OBJECT."
  (pcase-let* ((`(,workflow . ,keywords) (org-glance-config-migrate--org text))
               (tree (make-hash-table :test 'equal))
               (views (make-hash-table :test 'equal))
               (colors (make-hash-table :test 'equal)))
    (dolist (key '("todoCycles" "template"))
      (when-let* ((value (gethash key workflow))) (puthash key value object)))
    (dolist (field org-glance-config-migrate--tree-fields)
      (when-let* ((text (car (last (cdr (assoc (car field) keywords))))))
        (puthash (cadr field)
                 (pcase (nth 2 field)
                   ('integer
                    (unless (string-match-p "\\`[0-9]+\\'" text)
                      (org-glance-config--error "Invalid retired history depth"))
                    (string-to-number text))
                   ('boolean
                    (cond ((member (downcase text) '("true" "yes" "t" "1")) t)
                          ((member (downcase text) '("false" "no" "nil" "0")) :false)
                          (t (org-glance-config--error "Invalid retired autosync value"))))
                   (_ text)) tree)))
    (dolist (view org-glance-config-migrate--views)
      (when-let* ((value (car (last (cdr (assoc (car view) keywords))))))
        (puthash (cdr view) value views)))
    (dolist (line (cdr (assoc "GLANCE_STATE_COLORS" keywords)))
      (let* ((tokens (split-string line)) (theme (pop tokens))
             (pairs (make-hash-table :test 'equal)))
        (dolist (token tokens)
          (unless (string-match "\\`\\([^=]+\\)=\\(.+\\)\\'" token)
            (org-glance-config--error "Invalid retired state color"))
          (puthash (match-string 1 token) (match-string 2 token) pairs))
        (when theme (puthash theme pairs colors))))
    (puthash "tree" tree object)
    (puthash "views" views object)
    (puthash "stateColors" colors object)))

(cl-defun org-glance-config-migrate--layout (&key section text)
  "Convert SECTION's single data form in TEXT to JSON."
  (let* ((read-circle nil)
         (form (read-from-string text))
         (entries (car form))
         (object (make-hash-table :test 'equal)))
    (unless (string-match-p "\\`\\(?:[ \t\r\n]\\|;[^\n]*\\)*\\'"
                            (substring text (cdr form)))
      (org-glance-config--error "Retired layout contains trailing forms"))
    (unless (proper-list-p entries)
      (org-glance-config--error "Retired layout requires an association list"))
    (dolist (entry entries)
      (unless (and (consp entry) (stringp (car entry))
                   (eq :missing (gethash (car entry) object :missing)))
        (org-glance-config--error "Invalid or duplicate retired layout key"))
      (puthash (car entry) (org-glance-table-config:encode section (cdr entry)) object))
    object))

(defun org-glance-config-migrate--files (directory)
  "Return DIRECTORY's regular input files, refusing symbolic links."
  (when (file-symlink-p directory)
    (signal 'org-glance-config-unsafe (list directory)))
  (if (file-directory-p directory)
      (mapcan #'org-glance-config-migrate--files
              (directory-files directory t directory-files-no-dot-files-regexp))
    (unless (file-regular-p directory)
      (signal 'org-glance-config-unsafe (list directory)))
    (list directory)))

(defun org-glance-config-migrate:import (root)
  "Create ROOT's JSON settings from retired inputs after caller confirmation.
Return the published snapshot and retain every original input file.
Refuse existing JSON or an unsaved JSON draft."
  (let* ((directory (expand-file-name ".org-glance/config" root))
         (files (org-glance-config-migrate--files directory))
         (object (make-hash-table :test 'equal))
         (tags (make-hash-table :test 'equal))
         (layouts (make-hash-table :test 'equal))
         ignored)
    (puthash "format" 1 object)
    (dolist (path files)
      (let ((relative (file-relative-name path directory)))
        (cond
         ((equal relative "system.org")
          (org-glance-config-migrate--system (f-read-text path 'utf-8-unix) object))
         ((string-match "\\`tags/\\([^/]+\\)\\.org\\'" relative)
          (let ((tag (org-glance-config--tag-name (match-string 1 relative))))
            (when (gethash tag tags)
              (org-glance-config--error "Ambiguous retired tag names"))
            (puthash tag (car (org-glance-config-migrate--org
                              (f-read-text path 'utf-8-unix))) tags)))
         ((assoc relative org-glance-config-migrate--layouts)
          (let ((section (cdr (assoc relative org-glance-config-migrate--layouts))))
            (puthash section (org-glance-config-migrate--layout
                              :section section :text (f-read-text path 'utf-8-unix)) layouts)))
         (t (push relative ignored)))))
    (puthash "tags" tags object)
    (unless (gethash "todoCycles" object)
      (puthash "todoCycles" (org-glance-config-migrate--emacs-cycles) object))
    (when (> (hash-table-count layouts) 0)
      (let ((clients (make-hash-table :test 'equal)))
        (puthash "orgGlance" layouts clients)
        (puthash "clients" clients object)))
    (org-glance-config:decode (org-glance-config-file--make :object object))
    (when-let* ((buffer (find-buffer-visiting (org-glance-config-file:path root)))
                ((buffer-modified-p buffer)))
      (user-error "Save the config.json draft before migrating settings"))
    (prog1 (org-glance-config-file:write root :expected nil :change (lambda (_) object))
      (when ignored
        (display-warning 'org-glance
                         (format "Retired files retained without import: %s"
                                 (string-join (sort ignored #'string<) ", ")))))))

(provide 'org-glance-config-migrate)
;;; org-glance-config-migrate.el ends here
