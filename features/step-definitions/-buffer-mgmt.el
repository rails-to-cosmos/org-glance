;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(Then "^I should see$"
      (lambda (text)
        (should (s-contains-p text (buffer-substring-no-properties (point-min) (point-max))))))

(Then "^I should see \"\\(.+\\)\"$"
      (lambda (text)
        (Then "I should see" text)))

(And "^I should see message$"
     (lambda (text)
       (with-current-buffer "*Messages*"
         (goto-char (point-max))
         (Then "I should see" text))))

;; (When "^I run action \"\\(.+\\)\" for headlines and type \"\\(.+\\)\"$"
;;   (lambda (action user-input)
;;     (with-simulated-input user-input
;;       (cond ((string-equal action "visit") (org-glance-action-visit))))))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; ...
       ))

(Given "^I am in the buffer \"\\([^\"]+\\)\"$"
       (lambda (arg) (switch-to-buffer (get-buffer-create arg))))

(And "^I insert$"
     (lambda (arg)
       (insert arg)))
