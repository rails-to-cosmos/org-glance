(eval-when-compile
  (require 'load-relative)
  (require 'undercover)
  (require 'f))

(undercover "org-glance.el")

(defvar org-glance-test/test-path
  (->> (or load-file-name
           (__FILE__))
       file-name-directory
       directory-file-name)
  "Path to tests directory.")

(defvar org-glance-test/test-resources-path
  (f-join org-glance-test/test-path "resources")
  "Path to tests directory.")

(defvar org-glance-test/root-path
  (->> org-glance-test/test-path
       file-name-directory
       directory-file-name)
  "Path to root directory.")

(provide 'org-glance-test-init)
