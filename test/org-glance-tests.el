(require 'ert)
;; (load-file "org-glance.el") ;; for batch-mode
(require 'org-glance)

;; (defun trim-string (string)
;;   "Remove white spaces in beginning and ending of STRING.
;; White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
;;   (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;; (defun org-glance-headline-contains-tags-p (&rest tags)
;;   (equal (seq-intersection tags (org-get-tags)) tags))

(defmacro --org-glance (&rest body)
  "Open temp-file with org-glance prefix into a temporary buffer
execute BODY there like `progn', then kill the buffer and delete
the file returning the result of evaluating BODY."
  `(let ((fn (make-temp-file "org-glance-")))
     (find-file fn)
     (unwind-protect
         (progn
           (loop for symbol in '(,@body)
                 do (insert (symbol-name symbol)))
           (save-buffer)
           ;; (org-glance fn)
           )
       (save-buffer)
       (kill-buffer)
       (delete-file fn))))

(macroexpand (--org-glance
               * hello there
               :PROPERTIES:
               :HELLO: 1
               :HEY: 2
               :END:
               ** 2
               *** 3))

(ert-deftest org-glance-test--feature-provided ()
  (should (featurep 'org-glance)))

(defmacro with-temp-org-buffer (s &rest forms)
  "Create a temporary org-mode buffer with contents S and execute FORMS."
  `(with-temp-buffer
     (org-mode)
     (insert ,s)
     (goto-char 0)
     ,@forms))

(defun org-glance-test (&rest args)
  (save-excursion
    (with-temp-buffer
      (org-mode)
      (let* ((context (plist-get args :context))
             (input (plist-get args :input))
             (action (plist-get context :action))
             (expected (plist-get context :expected))
             (begin-marker (with-current-buffer (messages-buffer)
                             (point-max-marker)))
             (expression (format "(+ %i %i)" (random 10) (random 10)))
             (buffer (current-buffer))
             (org-confirm-elisp-link-function nil)
             (unread-command-events
              (listify-key-sequence
               (kbd (format "%s RET" input)))))
        (insert (format "* [[elisp:%s][%s]]" (org-link-escape expression) input))
        (apply 'org-glance context)
        (cond (action (= (funcall action) expected))
              (t (string= (format "%s => %s" expression (eval (read expression)))
                          (thread-first
                              (with-current-buffer (messages-buffer)
                                (buffer-substring begin-marker (point-max-marker)))
                            s-lines butlast -last-item trim-string))))))))

(ert-deftest org-glance-test/scopes-can-handle-nil-lambdas ()
  "Ignore nil lambdas in scopes."
  (should (->> (condition-case nil
                   (org-glance-scope-create (list (lambda () nil)))
                 (error nil))
               (-all? #'org-glance-scope-p))))

(ert-deftest org-glance-test/scopes-can-handle-unexisting-files ()
  "Unexisting file in `org-glance-scope-create` should create scope with current buffer."
  (let* ((temp-file (make-temp-file "org-glance-test-")))
    (delete-file temp-file)
    (should (->> temp-file
                 org-glance-scope-create
                 (-all? #'org-glance-scope-p)))))
