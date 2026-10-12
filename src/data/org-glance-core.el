;;; org-glance-core.el --- org-glance session state -*- lexical-binding: t; -*-

;; Low-level: `org-glance' requires this file, so it can never be required back.

;;; Code:

(require 'cl-lib)
(require 'org)                          ; `org-directory'

(defgroup org-glance nil "Projections over Org-mode headlines."
  :tag "Org Glance"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance'."
  :group 'org-glance
  :type 'directory)

(defvar org-glance-graph nil
  "Current global graph instance; nil until `org-glance-init' builds it.")

(cl-defun org-glance-initialized? ()
  "Return the global graph if the system is initialized, else nil."
  org-glance-graph)

(declare-function org-glance-init "org-glance")

(cl-defun org-glance-ensure-init ()
  "Return the global graph, running `org-glance-init' on first use."
  (or org-glance-graph
      (progn (org-glance-init) org-glance-graph)))

(cl-defun org-glance--batch-worker-load-paths (entry-point)
  "Return existing load paths for a batch worker running ENTRY-POINT.
Prefer newer sibling package directories so a live session survives an
in-place package upgrade."
  (let* ((loaded (symbol-file entry-point 'defun))
         (loaded-dir (and loaded (file-name-directory loaded)))
         (parent (and loaded-dir
                      (file-name-directory
                       (directory-file-name loaded-dir))))
         (installed
          (when (and parent (file-directory-p parent))
            (sort (directory-files parent t "\\`org-glance-[0-9]" t)
                  #'string>))))
    (delete-dups
     (cl-loop for path in (append installed load-path)
              when (and path (file-directory-p path))
              collect (expand-file-name path)))))

(cl-defun org-glance--batch-worker-command (entry-point args)
  "Return argv for an isolated batch Emacs calling ENTRY-POINT with ARGS."
  (let* ((source (symbol-file entry-point 'defun))
         (library (and source (file-name-base source)))
         (paths (org-glance--batch-worker-load-paths entry-point)))
    (unless library
      (error "Cannot locate batch worker entry point: %S" entry-point))
    (append
     (list (expand-file-name invocation-name invocation-directory)
           "--quick" "--batch")
     (cl-loop for path in paths append (list "-L" path))
     (list "-l" library "--eval"
           (prin1-to-string (cons entry-point args))))))

(defcustom org-glance-overview-default-view 'org-glance-table
  "Which view `org-glance-overview' opens by default.
`org-glance-table' (table dashboard) or `org-glance-overview' (org text); `T'
toggles it.  The legacy values `table' and `org' still work."
  :group 'org-glance
  :type '(choice (const :tag "Table dashboard (org-glance-table)" org-glance-table)
                 (const :tag "Org-text overview (org-glance-overview)" org-glance-overview)))

(cl-defun org-glance-overview--default-table? ()
  "Non-nil when `org-glance-overview-default-view' is the table dashboard."
  (memq org-glance-overview-default-view '(org-glance-table table)))

(provide 'org-glance-core)
