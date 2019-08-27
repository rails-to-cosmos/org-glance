(require 'ert)
(load-file "org-glance.el") ;; for batch-mode
(require 'org-glance)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defmacro with-temp-org-buffer (s &rest forms)
  "Create a temporary org-mode buffer with contents S and execute FORMS."
  `(save-excursion
     (with-temp-buffer
       (org-mode)
       (goto-char 0)
       (insert ,s)
       (goto-char 0)
       ,@forms)))

(defmacro org-glance--with-temp-filebuffer (&rest body)
  "Open temp-file with org-glance prefix into a temporary buffer
execute BODY there like `progn', then kill the buffer and delete
the file returning the result of evaluating BODY."
  `(save-window-excursion
     (let ((fn (make-temp-file "org-glance-")))
       (find-file fn)
       (unwind-protect
           ,@body
         (save-buffer)
         (kill-buffer)
         (delete-file fn)))))

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

(ert-deftest org-glance-test/scope-constructor-returns-list-of-fobs ()
  "Scope constructor should generate list of files-or-buffers."
  (should (equal (org-glance-scope-create (current-buffer)) (org-glance-scope-create (list (current-buffer)))))
  (should (equal (org-glance-scope-create (current-buffer)) (org-glance-scope-create (list (list (current-buffer)))))))

(ert-deftest org-glance-test/scopes-contain-no-duplicates ()
  "Scope should not contain duplicates."
  (let ((scopes
         (org-glance--with-temp-filebuffer
          (org-glance-scope-create
           `(;; buffer
             ,(current-buffer)
             ;; filename
             ,(buffer-file-name)
             ;; function symbol that returns buffer
             current-buffer
             ;; function that returns filename
             buffer-file-name)))))
    (should (= (length scopes) 1))))

(ert-deftest org-glance-test/scopes-can-handle-nil-lambdas ()
  "Ignore nil lambdas in scopes."
  (should (->> (condition-case nil
                   (org-glance-scope-create (list (lambda () nil)))
                 (error nil))
               null
               not)))

(ert-deftest org-glance-test/feature-provision ()
  (should (featurep 'org-glance)))
