(require 'org-glance)

(Given "^buffer \"\\([^\"]+\\)\"$"
       (lambda (buffer-name buffer-contents)
         (switch-to-buffer (get-buffer-create buffer-name))
         (org-mode)
         (delete-region (point-min) (point-max))
         (insert buffer-contents)))

(When "^I? ?insert \"\\([^\"]+\\)\"$"
  (lambda (thing)
    (insert thing)))

(When "^I? ?save buffer$"
  (lambda ()
    (save-buffer)))

(When "^I? ?switch to buffer \"\\([^\"]+\\)\"$" #'switch-to-buffer)

(When "^I? ?kill buffer$" #'kill-buffer)
(When "^I? ?kill current buffer$" #'kill-buffer)

(When "^I kill current line$"
  (lambda ()
    (kill-line)))

(When "^I? ?go ?to the beginning of buffer$"
  (lambda ()
    (goto-char (point-min))))

(When "^I? ?go ?to the end of the buffer$"
  (lambda ()
    (goto-char (point-max))))

(Then "^buffer string should be"
      (lambda (contents)
        (should (string=
                 (org-glance-test:normalize-string contents)
                 (org-glance-test:normalize-string (buffer-substring-no-properties (point-min) (point-max)))))))
