(require 'ert)

(defmacro with-temp-org-buffer (s &rest forms)
  "Create a temporary org-mode buffer with contents S and execute FORMS."
  `(save-excursion
     (with-temp-buffer
       (progn
         (org-mode)
         (goto-char 0)
         (insert ,s)
         (goto-char 0)
         ,@forms))))

(ert-deftest org-glance-test/handle-org-link ()
  "Test that we can handle org-links."
  (with-temp-org-buffer "* [[elisp:(+%201%202)][elisp]]"
    (let ((unread-command-events (listify-key-sequence (kbd "elisp RET")))
          (begin-marker (with-current-buffer (messages-buffer)
                          (point-max-marker))))
      (org-glance)
      (should (string= (string-trim (with-current-buffer (messages-buffer)
                                      (buffer-substring begin-marker (point-max))))
                       "(+ 1 2) => 3")))))

(ert-deftest org-glance-test/handle-property ()
  "Test that we can use handler property."
  (with-temp-org-buffer
"
* Title
:PROPERTIES:
:HANDLER: (+ 1 9)
:END:
"
(let ((unread-command-events (listify-key-sequence (kbd "tit RET"))))
  (should (= (org-glance) 10)))))

(ert-deftest org-glance-test/filter-removes-entries ()
  "Test filtering."
  (with-temp-org-buffer
"
* First
* Second
* Third
"
(let ((unread-command-events (listify-key-sequence (kbd "third RET"))))
  (should-error (org-glance :filter (lambda () (org-match-line "^.*Second")))))))

(ert-deftest org-glance-test/filter-doesnt-remove-suitable-entries ()
  "Test filtering."
  (with-temp-org-buffer
"
* First
* Second
* Third
"
(let ((unread-command-events (listify-key-sequence (kbd "sec RET"))))
  (should (eq nil (org-glance :filter (lambda () (org-match-line "^.*Second"))))))))
