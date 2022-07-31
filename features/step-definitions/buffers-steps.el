(And "^I save buffer$" #'save-buffer)

(And "^I kill buffer$" #'kill-buffer)

(And "^I goto the end of the buffer$"
     (lambda ()
       (goto-char (point-max))))

(And "^I go to the first headline$"
     (lambda ()
       (goto-char (point-min))
       (unless (org-at-heading-p)
         (outline-next-heading))))

(And "^I go to headline with title \"\\([^\"]+\\)\"$"
     (lambda (title)
       (goto-char (point-min))
       (search-forward (format "* %s" title))))

(Then "^buffer string should be"
      (lambda (contents)
        (should (string=
                 (org-glance-test:normalize-string contents)
                 (org-glance-test:normalize-string (buffer-substring-no-properties (point-min) (point-max)))))))

(And "^I print buffer contents$"
     (lambda ()
       (pp (buffer-string))))
