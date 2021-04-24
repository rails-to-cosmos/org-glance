;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)

(Given "^empty default scope"
       (lambda ()
         (setq org-glance-default-scope (list))))

(Given "^org-mode file"
       (lambda (text)
         (let ((temp-file (make-temp-file "org-glance-test-" nil ".org")))
           (message "Create file %s" temp-file)
           (message "Contents:\n%s" text)
           (find-file temp-file)
           (insert text)
           (save-buffer))))

(Then "^I add the file to scope"
     (lambda () (cl-pushnew (buffer-file-name) org-glance-default-scope)))

(Then "^I should have \\([[:digit:]]+\\) files? in scope$"
      (lambda (n) (should
              (= (length org-glance-default-scope)
                 (string-to-number n)))))

(Then "^I define view \"\\(.+\\)\" with default scope$"
      (lambda (view) (org-glance-def-view (intern view))))

(And "^I should see message$"
     (lambda (text)
       (let ((msg (with-current-buffer "*Messages*"
                    (goto-char (point-max))
                    (s-trim (thing-at-point 'line)))))
         (should (string= text msg)))))

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
