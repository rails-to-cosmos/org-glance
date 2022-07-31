(And "^I save buffer$" #'save-buffer)

(And "^I kill buffer$" #'kill-buffer)

(And "^I goto the end of the buffer$"
     (lambda ()
       (goto-char (point-max))))

(Then "^buffer string should be"
      (lambda (contents)
        (should (string=
                 (org-glance-test:normalize-string contents)
                 (org-glance-test:normalize-string (buffer-substring-no-properties (point-min) (point-max)))))))

(And "^I print buffer contents$"
     (lambda ()
       (pp (buffer-string))))
