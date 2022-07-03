(And "^I save buffer$" #'save-buffer)

(And "^I kill buffer$" #'kill-buffer)

(And "^I goto the end of the buffer$"
     (lambda ()
       (goto-char (point-max))))
