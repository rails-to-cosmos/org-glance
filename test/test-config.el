;;; test-config.el --- Typed settings and canonical Org context -*- lexical-binding: t -*-
(require 'ert)
(require 'org-glance-config)

(defun org-glance-config-test--settings (text)
  "Decode UTF-8 JSON TEXT into typed settings."
  (org-glance-config:decode
   (org-glance-config-file:decode (encode-coding-string text 'utf-8-unix))))

(ert-deftest org-glance-config-test-shared-settings-vectors ()
  (let ((path (getenv "GLANCE_CONFIG_SETTINGS_VECTORS")))
    (unless path (ert-skip "Set GLANCE_CONFIG_SETTINGS_VECTORS for paired typed fixtures"))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (entry (append (json-parse-buffer :null-object :null :false-object :false) nil))
        (ert-info ((gethash "name" entry))
          (let ((snapshot (org-glance-config-file:decode
                           (org-glance-config-file--serialize (gethash "config" entry)))))
            (if (eq t (gethash "invalid" entry))
                (should-error (org-glance-config:decode snapshot) :type 'org-glance-config-error)
              (let* ((settings (org-glance-config:decode snapshot))
                     (body (gethash "source" entry))
                     (tags (append (gethash "tags" entry) nil))
                     (presentation (org-glance-config:present settings tags body)))
                (should (equal (gethash "header" entry) (org-glance-org-presentation:header presentation)))
                (with-temp-buffer
                  (org-glance-config:insert-presentation presentation)
                  (should (equal (gethash "active" entry) (vconcat (delete-dups org-not-done-keywords))))
                  (should (equal (gethash "inactive" entry) (vconcat (delete-dups org-done-keywords))))
                  (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
                (should (= 4 (length (org-glance-config:fingerprints settings))))))))))))

(ert-deftest org-glance-config-test-read-only-prefix-and-span-boundary ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}"))
         (body "* PLAN 文🙂\r\nBody\r\n")
         (presentation (org-glance-config:present settings nil (concat (make-string 37 ?\s) body)
                                                 :start 37))
         (offset (length (org-glance-org-presentation:prefix presentation))))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string))))
      (should (member "PLAN" org-not-done-keywords))
      (should-not (member "PLAN" org-done-keywords))
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (goto-char (point-min))
      (should-error (insert "bad") :type 'text-read-only)
      (goto-char (+ (point-min) offset))
      (let ((org-inhibit-logging t)) (org-todo "DONE"))
      (should (string-prefix-p "* DONE" (org-glance-config:canonical-body presentation (buffer-string)))))
    (should (equal '(39 43 "DONE") (org-glance-config:map-edit presentation
                                                               :start (+ offset 2) :end (+ offset 6) :text "DONE")))
    (should-error (org-glance-config:map-edit presentation :start 0 :end offset :text "")
                  :type 'org-glance-config-error)
    (should-error (org-glance-config:canonical-body presentation (concat "bad" body))
                  :type 'org-glance-config-error)))

(ert-deftest org-glance-config-test-source-headers-remain-owned-and-refreshable ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
         (body "#+TODO: PLAN | END\n#+PROPERTY: Owner 文\n* PLAN Title\n")
         (presentation (org-glance-config:present settings nil body)))
    (should (equal "" (org-glance-org-presentation:prefix presentation)))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal body (buffer-string)))
      (org-set-regexps-and-options)
      (should (member "PLAN" org-not-done-keywords))
      (should-not (member "PLAN" org-done-keywords))
      (goto-char (point-max)) (insert "Authored\n")
      (should (equal (concat body "Authored\n")
                     (org-glance-config:canonical-body presentation (buffer-string)))))))

(ert-deftest org-glance-config-test-decoding-and-fingerprints-observe-no-files ()
  (cl-letf (((symbol-function 'directory-files) (lambda (&rest _) (ert-fail "Settings enumerated a directory")))
            ((symbol-function 'insert-file-contents) (lambda (&rest _) (ert-fail "Settings read a file")))
            ((symbol-function 'insert-file-contents-literally) (lambda (&rest _) (ert-fail "Settings read raw bytes"))))
    (let* ((a (org-glance-config-test--settings "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
           (b (org-glance-config-test--settings " {\"clients\":{\"glance\":{\"theme\":\"dark\"}},\"todoCycles\":[{\"active\":[\"PLAN\"]}],\"format\":1}\n"))
           (c (org-glance-config-test--settings "{\"format\":1,\"todoCycles\":[{\"inactive\":[\"PLAN\"]}]}")))
      (should (equal (org-glance-config:fingerprints a) (org-glance-config:fingerprints b)))
      (should (equal (car (org-glance-config:fingerprints a)) (car (org-glance-config:fingerprints c))))
      (should-not (equal (cadr (org-glance-config:fingerprints a)) (cadr (org-glance-config:fingerprints c)))))))

(ert-deftest org-glance-config-test-fingerprints-preserve-the-settings-object ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"tags\":{\"book\":{\"todoCycles\":[{\"active\":[\"READING\"],\"inactive\":[\"READ\"]}]}}}"))
         (before (copy-tree (org-glance-config:keywords settings nil '("book"))))
         (fingerprints (org-glance-config:fingerprints settings)))
    (dotimes (_ 3)
      (should (equal fingerprints (org-glance-config:fingerprints settings)))
      (should (equal before (org-glance-config:keywords settings nil '("book")))))))

(ert-deftest org-glance-config-test-unknown-fields-are-diagnosed-and-retained ()
  (let* ((snapshot (org-glance-config-file:decode
                    "{\"format\":1,\"futureSetting\":{\"keep\":true},\"clients\":{\"other\":{\"value\":17}}}"))
         (before (org-glance-config-file--serialize (org-glance-config-file:object snapshot)))
         (settings (org-glance-config:decode snapshot)))
    (should (equal '("unknown setting: futureSetting") (org-glance-settings:diagnostics settings)))
    (org-glance-config:fingerprints settings)
    (should (equal before (org-glance-config-file--serialize (org-glance-config-file:object snapshot))))))

(ert-deftest org-glance-config-test-overview-refuses-incompatible-scopes ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"tags\":{\"a\":{\"todoCycles\":[{\"active\":[\"WAIT\"]}]},\"b\":{\"todoCycles\":[{\"inactive\":[\"WAIT\"]}]}}}"))
         (body "* WAIT Title\n")
         (a (org-glance-config:present settings '("a") body))
         (b (org-glance-config:present settings '("b") body)))
    (should (equal (org-glance-org-presentation:header a)
                   (org-glance-config:compatible-header (list (cons "a.org#1" a) (cons "a.org#2" a)))))
    (let ((error (should-error
                  (org-glance-config:compatible-header (list (cons "a.org#1" a) (cons "b.org#2" b)))
                  :type 'org-glance-config-error)))
      (should (string-match-p "a.org#1 and b.org#2" (error-message-string error))))))

(ert-deftest org-glance-config-test-file-context-outside-the-canonical-body ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
         (before "#+TODO: PLAN(p) | END(e)\r\n#+PROPERTY: Owner 文\r\n")
         (body "* PLAN Title文\r\n")
         (after "* DONE Sibling\r\n")
         (presentation (org-glance-config:present settings nil (concat before body after)
                         :start (length before) :end (+ (length before) (length body)))))
    (should (equal "#+TODO: TODO PLAN(p) | DONE END(e)\n"
                   (org-glance-org-presentation:header presentation)))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
    (should (equal (list (length before) (+ (length before) 1) "!")
                   (org-glance-config:map-edit presentation
                    :start (length (org-glance-org-presentation:prefix presentation))
                    :end (1+ (length (org-glance-org-presentation:prefix presentation))) :text "!")))))

(provide 'test-config)
;;; test-config.el ends here
