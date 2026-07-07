;;; verify-fat.el --- prove dist/org-glance.el is self-contained -*- lexical-binding: t; -*-

;; Remove the multi-directory sources (src/data, src/view, and the repo root that
;; holds the original org-glance.el) from `load-path', keeping the external
;; dependencies under .eask.  Then byte-compile and load the fat file with ONLY
;; itself + its deps reachable.  A wrong concatenation order (a macro used before
;; it is defined) fails here instead of silently falling back to the sources.

(let* ((root (directory-file-name (file-truename default-directory)))
       (dist (expand-file-name "dist" root)))
  (setq load-path
        (cons dist
              (seq-remove (lambda (p)
                            (let ((tp (file-truename p)))
                              (or (string-match-p "/src/\\(data\\|view\\)/?$" tp)
                                  (string= (directory-file-name tp) root))))
                          load-path)))
  (require 'bytecomp)
  (setq byte-compile-error-on-warn t)
  (unless (byte-compile-file (expand-file-name "org-glance.el" dist))
    (error "fat: byte-compile failed"))
  (load (expand-file-name "org-glance.elc" dist) nil t)
  (dolist (cmd '(org-glance-materialize org-glance-open org-glance-extract))
    (unless (commandp cmd)
      (error "fat: command `%s' missing after standalone load" cmd)))
  (message "fat OK: dist/org-glance.el compiles + loads standalone"))

;;; verify-fat.el ends here
