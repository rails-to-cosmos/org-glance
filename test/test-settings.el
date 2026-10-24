;;; test-settings.el --- JSON settings editor contracts -*- lexical-binding: t -*-

(require 'test-helpers)
(require 'org-glance-settings)

(defun org-glance-settings-test--publish (graph text)
  "Publish JSON TEXT in GRAPH and observe it."
  (org-glance-test:write (org-glance-config-file:path (org-glance-graph:directory graph)) text)
  (org-glance-config:refresh graph))

(defun org-glance-settings-test--object (graph)
  "Read GRAPH's saved configuration object."
  (org-glance-config-file:object (org-glance-config-file:read (org-glance-graph:directory graph))))

(defmacro org-glance-settings-test--with-editor (tag json &rest body)
  "Open TAG's settings over JSON in a disposable GRAPH, then run BODY."
  (declare (indent 2))
  `(org-glance-test:with-graph graph
     (org-glance-settings-test--publish graph ,json)
     (save-window-excursion
       (let ((buffer (org-glance-settings:visit graph ,tag)))
         (unwind-protect
             (with-current-buffer buffer ,@body)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))))))

(ert-deftest org-glance-settings-test-workflow-writer-accepts-observed-absence ()
  (org-glance-test:with-graph graph
    (let* ((root (org-glance-graph:directory graph))
           (path (org-glance-config-file:path root))
           (values (make-hash-table :test 'equal)))
      (puthash "title" "First book" values)
      (should-error (org-glance-config:write-workflow graph "book" values)
                    :type 'user-error)
      (should-error (org-glance-config:write-workflow graph "book" values :expected "")
                    :type 'user-error)
      (should-not (file-exists-p path))
      (let* ((saved (org-glance-config:write-workflow graph "book" values :expected nil))
             (bytes (with-temp-buffer (insert-file-contents-literally path) (buffer-string))))
        (should (stringp (org-glance-config-file:revision saved)))
        (should (equal "First book"
                       (gethash "title" (gethash "book" (gethash "tags" (org-glance-config-file:object saved))))))
        (puthash "title" "Stale replacement" values)
        (should-error (org-glance-config:write-workflow graph "book" values :expected nil)
                      :type 'org-glance-config-conflict)
        (should (equal bytes (with-temp-buffer (insert-file-contents-literally path) (buffer-string))))))))

(ert-deftest org-glance-settings-test-scope-writer-preserves-peers-and-case ()
  (org-glance-test:with-graph graph
    (org-glance-settings-test--publish
     graph "{\"format\":1,\"tags\":{\"Book\":{\"title\":\"Old\",\"template\":\"Keep\"},\"other\":{\"storage\":\"local\"}},\"clients\":{\"glance\":{\"theme\":\"dark\"}},\"extension\":42}")
    (let* ((before (org-glance-config-file:read (org-glance-graph:directory graph)))
           (values (org-glance-settings--scope before "book")))
      (puthash "title" "New 文" values)
      (org-glance-config:write-workflow graph "BOOK" values :expected (org-glance-config-file:revision before))
      (let* ((object (org-glance-settings-test--object graph)) (tags (gethash "tags" object)))
        (should (equal "New 文" (gethash "title" (gethash "Book" tags))))
        (should (equal "Keep" (gethash "template" (gethash "Book" tags))))
        (should-not (gethash "book" tags))
        (should (equal "local" (gethash "storage" (gethash "other" tags))))
        (should (equal "dark" (gethash "theme" (gethash "glance" (gethash "clients" object)))))
        (should (= 42 (gethash "extension" object))))
      (should-error (org-glance-config:write-workflow graph "book" values
                                                    :expected (org-glance-config-file:revision before))
                    :type 'org-glance-config-conflict))))

(ert-deftest org-glance-settings-test-one-save-publishes-compound-draft ()
  (org-glance-settings-test--with-editor "book"
      "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Old\",\"template\":\"* %?\\r\\nBody 文\"}},\"clients\":{\"glance\":{\"theme\":\"light\"}}}"
    (let ((before (org-glance-config-file:revision
                   (org-glance-config-file:read (org-glance-graph:directory graph)))))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "New")))
        (org-glance-settings--edit "title" nil))
      (org-glance-settings--edit "todoCycles" nil)
      (let ((answers '("PLAN(p)" "END(e)")))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers))))
          (org-glance-settings--add nil nil)))
      (org-glance-settings--back nil nil)
      (org-glance-settings--edit "template" nil)
      (table-view--goto-id "1") (table-view--goto-cell "text")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Changed 文")))
        (org-glance-settings--edit "1" nil))
      (should (equal before (org-glance-config-file:revision
                             (org-glance-config-file:read (org-glance-graph:directory graph)))))
      (let ((publish (symbol-function 'org-glance-config-file--publish)) (writes 0))
        (cl-letf (((symbol-function 'org-glance-config-file--publish)
                   (lambda (&rest args) (cl-incf writes) (apply publish args))))
          (org-glance-settings--save nil nil))
        (should (= 1 writes)))
      (let ((saved (gethash "book" (gethash "tags" (org-glance-settings-test--object graph)))))
        (should (equal "New" (gethash "title" saved)))
        (should (equal "* %?\r\nChanged 文" (gethash "template" saved)))
        (should (equal ["PLAN(p)"] (gethash "active" (aref (gethash "todoCycles" saved) 0)))))
      (should-not (org-glance-settings--dirty org-glance-settings--session)))))

(ert-deftest org-glance-settings-test-stale-save-retains-draft-until-explicit-reload ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1}"
    (let ((session org-glance-settings--session))
      (puthash "title" "Draft" (org-glance-settings-draft-values session))
      (org-glance-settings-test--publish graph "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Peer\"}}}")
      (should-error (org-glance-settings--save nil nil) :type 'org-glance-config-conflict)
      (should (equal "Draft" (gethash "title" (org-glance-settings-draft-values session))))
      (should (equal "Peer" (gethash "title" (gethash "book" (gethash "tags" (org-glance-settings-test--object graph))))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
        (should (condition-case nil (progn (org-glance-settings--reload nil nil) nil) (quit t))))
      (should (eq session org-glance-settings--session))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "current")))
        (org-glance-settings--reload nil nil))
      (should (equal "Peer" (gethash "title" (org-glance-settings-draft-values org-glance-settings--session))))
      (should-not (org-glance-settings--dirty org-glance-settings--session)))))

(ert-deftest org-glance-settings-test-rebase-keeps-draft-and-adopts-unedited-peer-fields ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Old\"}}}"
    (puthash "title" "Draft" (org-glance-settings-draft-values org-glance-settings--session))
    (org-glance-settings-test--publish
     graph "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Peer\",\"template\":\"Peer template\"}}}")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices _predicate require-match &rest _)
                 (should require-match) (should (equal '("current" "draft") choices)) "draft")))
      (org-glance-settings--reload nil nil))
    (let ((values (org-glance-settings-draft-values org-glance-settings--session)))
      (should (equal "Draft" (gethash "title" values)))
      (should (equal "Peer template" (gethash "template" values))))
    (org-glance-settings--save nil nil)
    (let ((saved (gethash "book" (gethash "tags" (org-glance-settings-test--object graph)))))
      (should (equal "Draft" (gethash "title" saved)))
      (should (equal "Peer template" (gethash "template" saved))))))

(ert-deftest org-glance-settings-test-failed-apply-keeps-saved-revision-for-retry ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1}"
    (puthash "title" "Durable" (org-glance-settings-draft-values org-glance-settings--session))
    (cl-letf (((symbol-function 'org-glance-config:refresh) (lambda (&rest _) (error "Injected apply failure"))))
      (should-error (org-glance-settings--save nil nil)))
    (let ((receipt (org-glance-settings-draft-revision org-glance-settings--session))
          (publish (symbol-function 'org-glance-config-file--publish)) (writes 0))
      (should (equal receipt (org-glance-config-file:revision
                              (org-glance-config-file:read (org-glance-graph:directory graph)))))
      (should (org-glance-settings--dirty org-glance-settings--session))
      (cl-letf (((symbol-function 'org-glance-config-file--publish)
                 (lambda (&rest args) (cl-incf writes) (apply publish args))))
        (org-glance-settings--save nil nil))
      (should (= writes 0))
      (should-not (org-glance-settings--dirty org-glance-settings--session)))))

(ert-deftest org-glance-settings-test-parent-filter-mark-and-cell-are-restored ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1}"
    (table-view-filter "TODO cycles")
    (table-view--goto-id "todoCycles") (table-view--goto-cell "value")
    (table-view-mark-toggle)
    (table-view--goto-id "todoCycles") (table-view--goto-cell "value")
    (let ((point (org-glance-view:point-context)))
      (org-glance-settings--edit "todoCycles" nil)
      (should (equal "todoCycles" org-glance-settings--page))
      (org-glance-settings--back nil nil)
      (should (equal "TODO cycles" table-view--filter))
      (should (equal '("todoCycles") table-view--marks))
      (should (equal point (org-glance-view:point-context))))))

(ert-deftest org-glance-settings-test-reset-noop-invalid-candidate-and-line-endings ()
  (org-glance-settings-test--with-editor nil "{\"format\":1,\"extension\":42}"
    (let ((publish (symbol-function 'org-glance-config-file--publish)) (writes 0))
      (org-glance-settings--reset "template" nil)
      (cl-letf (((symbol-function 'org-glance-config-file--publish)
                 (lambda (&rest args) (cl-incf writes) (apply publish args))))
        (org-glance-settings--save nil nil))
      (should (= 0 writes)))
    (puthash "todoCycles" [nil] (org-glance-settings-draft-values org-glance-settings--session))
    (should-error (org-glance-settings--save nil nil) :type 'org-glance-config-error)
    (should-not (gethash "todoCycles" (org-glance-settings-test--object graph)))
    (remhash "todoCycles" (org-glance-settings-draft-values org-glance-settings--session))
    (org-glance-settings--edit "template" nil)
    (should (equal '("inherited" "new") (mapcar (lambda (row) (alist-get 'id row)) table-view--rows)))
    (should-error (org-glance-settings--delete "inherited" nil) :type 'user-error)
    (should-not (gethash "template" (org-glance-settings-draft-values org-glance-settings--session)))
    (dolist (text '("" "a" "a\r\n\r\nb\r" "文\n🙂\r\nlast"))
      (let ((items (org-glance-settings--lines text)))
        (org-glance-settings--set-items items)
        (should (equal text (gethash "template" (org-glance-settings-draft-values org-glance-settings--session))))))
    (should-error (org-glance-settings--set-items '(("one" . "") ("two" . "\n"))) :type 'user-error)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-not (org-glance-settings--kill-query)))))

(ert-deftest org-glance-settings-test-line-decomposition-law-covers-mixed-input ()
  (let ((seed 42) (seen (make-hash-table :test 'equal)) (alphabet [97 13 10 20013]))
    (dotimes (n 120)
      (let* ((text (apply #'string
                          (cl-loop repeat (mod n 33) collect
                                   (progn (setq seed (mod (+ (* seed 1664525) 1013904223) 4294967296))
                                          (aref alphabet (mod (/ seed 256) 4))))))
             (lines (org-glance-settings--lines text)))
        (should (equal text (mapconcat (lambda (line) (concat (car line) (cdr line))) lines "")))
        (dolist (line lines)
          (should-not (string-match-p "[\r\n]" (car line)))
          (should (rassoc (cdr line) org-glance-settings--endings))
          (puthash (cdr line) t seen))
        (dolist (line (butlast lines)) (should-not (equal (cdr line) "")))))
    (dolist (ending '("" "\r" "\n" "\r\n")) (should (gethash ending seen)))))

(ert-deftest org-glance-settings-test-appended-line-keeps-text-and-uses-existing-ending ()
  (org-glance-settings-test--with-editor "book"
      "{\"format\":1,\"tags\":{\"book\":{\"template\":\"first\\r\\nlast\"}}}"
    (org-glance-settings--edit "template" nil)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "new")))
      (org-glance-settings--add nil nil))
    (should (equal "first\r\nlast\r\nnew\r\n"
                   (gethash "template" (org-glance-settings-draft-values org-glance-settings--session))))
    (table-view--goto-id "2") (table-view--goto-cell "order")
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1")))
      (org-glance-settings--edit "2" nil))
    (org-glance-settings--delete "1" nil)
    (should (equal "new\r\nlast\r\n"
                   (gethash "template" (org-glance-settings-draft-values org-glance-settings--session))))))

(ert-deftest org-glance-settings-test-tag-entrypoint-and-existing-draft-reentry ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1}"
    (let ((org-glance-graph graph) (session org-glance-settings--session))
      (puthash "title" "Keep draft" (org-glance-settings-draft-values session))
      (org-glance-tag-config-edit "book")
      (should (eq session org-glance-settings--session))
      (should (equal "Keep draft" (gethash "title" (org-glance-settings-draft-values session)))))))

(ert-deftest org-glance-settings-test-empty-completion-preserves-storage-and-ending ()
  (org-glance-settings-test--with-editor "book"
      "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Old\",\"template\":\"Keep\\r\\n\"}}}"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (should-error (org-glance-settings--edit "storage" nil) :type 'user-error)
      (should-not (gethash "storage" (org-glance-settings-draft-values org-glance-settings--session)))
      (org-glance-settings--edit "template" nil)
      (table-view--goto-id "0") (table-view--goto-cell "ending")
      (should-error (org-glance-settings--edit "0" nil) :type 'user-error)
      (should (equal "Keep\r\n" (gethash "template" (org-glance-settings-draft-values org-glance-settings--session))))
      (org-glance-settings--back nil nil))))

(ert-deftest org-glance-settings-test-empty-conflict-choice-retains-entire-draft ()
  (org-glance-settings-test--with-editor "book"
      "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Old\"}}}"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (let ((session org-glance-settings--session))
        (puthash "title" "Draft" (org-glance-settings-draft-values session))
        (org-glance-settings-test--publish graph "{\"format\":1,\"tags\":{\"book\":{\"title\":\"Peer\"}}}")
        (should-error (org-glance-settings--reload nil nil) :type 'user-error)
        (should (eq session org-glance-settings--session))
        (should (equal "Draft" (gethash "title" (org-glance-settings-draft-values session))))))))

(ert-deftest org-glance-settings-test-system-and-named-system-tag-have-distinct-drafts-and-saves ()
  (dolist (first '(nil "system"))
    (org-glance-settings-test--with-editor first
        "{\"format\":1,\"template\":\"System\",\"tags\":{\"system\":{\"template\":\"Tag\"}}}"
      (let* ((first-buffer (current-buffer)) (first-session org-glance-settings--session)
             (second (if first nil "system")) (second-buffer nil))
        (puthash "template" "Retained first draft" (org-glance-settings-draft-values first-session))
        (unwind-protect
            (progn
              (setq second-buffer (org-glance-settings:visit graph second))
              (should-not (eq first-buffer second-buffer))
              (should (equal second (org-glance-settings-draft-tag org-glance-settings--session)))
              (should (equal (if second "Tag" "System")
                             (gethash "template" (org-glance-settings-draft-values org-glance-settings--session))))
              (puthash "template" "Saved second" (org-glance-settings-draft-values org-glance-settings--session))
              (org-glance-settings--save nil nil)
              (let ((object (org-glance-settings-test--object graph)))
                (should (equal (if second "System" "Saved second") (gethash "template" object)))
                (should (equal (if second "Saved second" "Tag")
                               (gethash "template" (gethash "system" (gethash "tags" object))))))
              (should (eq first-buffer (org-glance-settings:visit graph first)))
              (should (eq first-session org-glance-settings--session))
              (should (equal "Retained first draft" (gethash "template" (org-glance-settings-draft-values first-session))))
              (org-glance-settings--reload nil nil)
              (org-glance-settings--save nil nil)
              (let ((object (org-glance-settings-test--object graph)))
                (should (equal (if first "Saved second" "Retained first draft") (gethash "template" object)))
                (should (equal (if first "Retained first draft" "Saved second")
                               (gethash "template" (gethash "system" (gethash "tags" object)))))))
          (when (and second-buffer (not (eq second-buffer first-buffer)) (buffer-live-p second-buffer))
            (with-current-buffer second-buffer
              (let ((kill-buffer-query-functions nil)) (kill-buffer second-buffer)))))))))

(ert-deftest org-glance-settings-test-refuses-unowned-buffer-reuse ()
  (org-glance-settings-test--with-editor "book" "{\"format\":1}"
    (let ((text (buffer-string)))
      (setq org-glance-settings--session nil)
      (should-error (org-glance-settings:visit graph "book") :type 'user-error)
      (should (equal text (buffer-string))))))

(provide 'test-settings)
;;; test-settings.el ends here
