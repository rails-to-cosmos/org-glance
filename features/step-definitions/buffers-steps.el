(When "^I save buffer$" #'save-buffer)
(When "^I kill buffer$" #'kill-buffer)
(When "^I kill current buffer$" #'kill-buffer)
(When "^I goto the end of the buffer$" #'end-of-buffer)
(When "^I print buffer contents$" (lambda () (pp (buffer-string))))

(Then "^buffer string should be"
      (lambda (contents)
        (should (string=
                 (org-glance-test:normalize-string contents)
                 (org-glance-test:normalize-string (buffer-substring-no-properties (point-min) (point-max)))))))
