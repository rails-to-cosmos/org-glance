(When "^I? ?insert \"\\([^\"]+\\)\"$"
  (lambda (thing)
    (insert thing)
    (org-glance-material-overlay-manager-redisplay)))

(When "^I? ?save buffer$" #'save-buffer)
(When "^I? ?kill buffer$" #'kill-buffer)
(When "^I? ?kill current buffer$" #'kill-buffer)

(When "^I? ?goto the end of the buffer$"
  (lambda ()
    (goto-char (point-max))))

(Then "^buffer string should be"
      (lambda (contents)
        (should (string=
                 (org-glance-test:normalize-string contents)
                 (org-glance-test:normalize-string (buffer-substring-no-properties (point-min) (point-max)))))))
