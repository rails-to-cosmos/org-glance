(And "^I save current buffer$" #'save-buffer)

(And "^I goto the end of the buffer$"
     (lambda ()
       (goto-char (point-max))))
