(require 'ert)

(defmacro with-temp-org-buffer (s &rest forms)
  "Create a temporary org-mode buffer with contents S and execute FORMS."
  `(save-excursion
     (with-temp-buffer
       (org-mode)
       (goto-char 0)
       (insert ,s)
       (goto-char 0)
       ,@forms)))

(defun org-glance-predicate/can-handle-org-links ()
  "Can we handle org-links?"
  (with-temp-org-buffer "* [[elisp:(+%201%202)][elisp]]"
                        (let ((unread-command-events (listify-key-sequence (kbd "elisp RET")))
                              (begin-marker (with-current-buffer (messages-buffer)
                                              (point-max-marker))))
                          (org-glance)
                          (string= (string-trim (with-current-buffer (messages-buffer)
                                                  (buffer-substring begin-marker (point-max))))
                                   "(+ 1 2) => 3"))))

(defun org-glance-test-explainer/can-handle-org-links ()
  (message "Handling org-links feature doesn't work properly"))

(put 'org-glance-predicate/can-handle-org-links
     'ert-explainer
     'org-glance-test-explainer/can-handle-org-links)

(ert-deftest org-glance-test/can-handle-org-links ()
  "Test that we can handle org-links."
  (should (org-glance-predicate/can-handle-org-links)))

(ert-deftest org-glance-test/can-handle-default-property ()
  "Test that we can use default handler property."
  (with-temp-org-buffer
"
* Title
:PROPERTIES:
:HANDLER: (+ 1 9)
:END:
"
(let ((unread-command-events (listify-key-sequence (kbd "tit RET"))))
  (should (= (org-glance) 10)))))

(ert-deftest org-glance-test/can-handle-custom-property ()
  "Test that we can use custom handler property."
  (with-temp-org-buffer
"
* Title
:PROPERTIES:
:CUSTOM_HANDLER: (+ 1 11)
:END:
"
(let ((unread-command-events (listify-key-sequence (kbd "tit RET"))))
  (should (= (org-glance :handler "CUSTOM_HANDLER") 12)))))

(defun org-glance-predicate/can-handle-symbolic-property ()
  "Can we handle symbolic property as org-babel block name?"
  (with-temp-org-buffer
   "
* Please, handle custom block
:PROPERTIES:
:CUSTOM_HANDLER: custom-block
:END:

#+NAME: custom-block
#+BEGIN_SRC emacs-lisp
(+ 15 16)
#+END_SRC
"
   (let ((unread-command-events (listify-key-sequence (kbd "Plea RET"))))
     (= (org-glance :handler "CUSTOM_HANDLER") 31))))

(defun org-glance-test-explainer/can-handle-symbolic-property ()
  (message "Failed to handle symbolic property as org-babel block name"))

(put 'org-glance-predicate/can-handle-symbolic-property
     'ert-explainer
     'org-glance-test-explainer/can-handle-symbolic-property)

(ert-deftest org-glance-test/can-handle-symbolic-property ()
  "Test that we can handle symbolic properties."
  (should (org-glance-predicate/can-handle-symbolic-property)))

(ert-deftest org-glance-test/filter-removes-entries ()
  "Test filtering."
  (with-temp-org-buffer
"
* First
* Second
* Third
* Security
"
(let ((unread-command-events (listify-key-sequence (kbd "third RET"))))
  (should-error (org-glance :filter (lambda () (org-match-line "^.*Sec")))))))

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
