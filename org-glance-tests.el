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
      (let ((begin-marker (with-current-buffer (messages-buffer)
                            (point-max-marker)))
            (context (plist-get args :context))
            (expression (format "(+ %i %i)" (random 10) (random 10)))
            (input (plist-get args :input)))

        (let* ((buffer (current-buffer))
               (org-confirm-elisp-link-function nil)
               (unread-command-events
                (listify-key-sequence
                 (kbd (format "%s RET" input)))))
          (insert (format "* [[elisp:%s][%s]]" (org-link-escape expression) input))
          (apply 'org-glance context))

        (string= (format "%s => %s" expression (eval (read expression)))
                 (trim-string
                  (-last-item
                   (butlast
                    (s-lines
                     (with-current-buffer (messages-buffer)
                       (buffer-substring begin-marker (point-max))))))))))))

(ert-deftest org-glance-test/can-work-with-empty-cache-file ()
  "Should work with empty cache file."
  (should
   (org-glance-test
    :context '(:no-cache-file t)
    :input "Hello")))

(ert-deftest org-glance-test/can-handle-org-links ()
  "Test that we can handle org-links."
  (should
   (org-glance-test
    :context '(:no-cache-file t)
    :input "elisp-link")))

(ert-deftest org-glance-test/compl-non-file-buffer ()
  "Should work properly from non-file buffers."
  (should
   (org-glance-test
    :context '(:no-cache-file t
               :inplace t
               :scope (list buffer))
    :input "elisp-link")))

(ert-deftest org-glance-test/scopes-contain-no-duplicates ()
  "Scope should not contain duplicates."
  (let ((scopes
         (org-glance--with-temp-filebuffer
          (org-glance--aggregate-scopes
           (list
            ;; buffer
            (current-buffer)

            ;; filename
            (buffer-file-name)

            ;; function that returns buffer
            'current-buffer

            ;; function that returns filename
            'buffer-file-name)))))
    (should (= (length scopes) 1))))

(ert-deftest org-glance-test/scopes-can-handle-nil-lambdas ()
  "Ignore nil lambdas in scopes."
  (should
   (not (null
         (condition-case nil
             (org-glance--aggregate-scopes (list (lambda () nil)))
           (error nil))))))

(defun org-glance-req/filter-produces-proper-predicates-p (input expected)
  "Can we split user filter into atomic predicates?"
  (equal (org-glance--filter-predicates input) expected))

(defun org-glance-test-explainer/filter-produces-proper-predicates (filter expected)
  (cond ((functionp filter) "Unable to resolve lambda filter")
        ((symbolp filter) "Unable to resolve symbolic filter")
        ((stringp filter) "Unable to resolve string filter")
        ((listp filter) (cl-loop for elt in filter
                                 when (functionp elt) return "Unable to resolve lambda from filter list"
                                 when (symbolp elt)   return "Unable to resolve symbol from filter list"
                                 when (stringp elt)   return "Unable to resolve string from filter list"))
        (t "Unrecognized filter must raise an error")))

(put 'org-glance-req/filter-produces-proper-predicates-p
     'ert-explainer
     'org-glance-test-explainer/filter-produces-proper-predicates)

(ert-deftest org-glance-test/filter-produces-proper-predicates-lambda ()
  (should (org-glance-req/filter-produces-proper-predicates-p
           (lambda () t) '((lambda () t)))))

(ert-deftest org-glance-test/filter-produces-proper-predicates-symbol ()
  (should (org-glance-req/filter-produces-proper-predicates-p
           'links (list (alist-get 'links org-glance/default-filters)))))

(ert-deftest org-glance-test/filter-produces-proper-predicates-string ()
  (should (org-glance-req/filter-produces-proper-predicates-p
           "links" (list (alist-get 'links org-glance/default-filters)))))

(ert-deftest org-glance-test/filter-produces-proper-predicates-list ()
  (should (org-glance-req/filter-produces-proper-predicates-p
           (list 'links (lambda () t) "links")
           (list (alist-get 'links org-glance/default-filters)
                 (lambda () t)
                 (alist-get 'links org-glance/default-filters)))))

(ert-deftest org-glance-test/filter-removes-entries ()
  "Test filtering."
  (should
   (condition-case nil
        (org-glance-test
         :context (list :no-cache-file t
                        :inplace t
                        :filter (lambda () (org-match-line "^ example$")))
         :input "elisp-link")
     (error t))))

(ert-deftest org-glance-test/filter-doesnt-remove-suitable-entries ()
  (should
   (org-glance-test
    :context (list :no-cache-file t
                   :inplace t
                   :filter (lambda () (org-match-line "^.*elisp-link.*$")))
    :input "elisp-link")))

(ert-deftest org-glance-test/feature-provision ()
  (should (featurep 'org-glance)))
