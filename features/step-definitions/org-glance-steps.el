;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)

(Given "^empty scope"
       (lambda ()
         (setq org-glance-default-scope (list))))

(Given "^file with contents"
       (lambda (text)
         (let ((temp-file (make-temp-file "org-glance-test-" nil ".org")))
           (message "Create file %s" temp-file)
           (message "Contents:\n%s" text)
           (find-file temp-file)
           (insert text)
           (save-buffer))))

(Then "^I add the file to scope"
     (lambda ()
       (cl-pushnew (buffer-file-name) org-glance-default-scope)))

(Then "^I should have \\([[:digit:]]+\\) file in scope$"
  (lambda (n) (= (length org-glance-default-scope) (string-to-number n))))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Given "^I am in the buffer \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (switch-to-buffer (get-buffer-create arg))
    ))

(And "^I insert$"
  (lambda (arg)
    (insert arg)))
