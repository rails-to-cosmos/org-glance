(Given "^file \"\\([^\"]+\\)\"$"
       (lambda (filename contents)
         (puthash filename
                  (make-temp-file filename nil ".org" contents)
                  ecukes--files)))

(Then "^I find file \"\\([^\"]+\\)\"$"
      (lambda (filename)
        (find-file (gethash filename ecukes--files))))
