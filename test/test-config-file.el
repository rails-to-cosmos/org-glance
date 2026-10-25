;;; test-config-file.el --- Shared configuration transactions -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-config-file)

(defmacro org-glance-config-test--with-root (&rest body)
  "Run BODY with ROOT bound to an isolated directory."
  (declare (indent 0))
  `(let ((root (make-temp-file "org-glance-config-test-" t)))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(defun org-glance-config-test--put (key value)
  "Return a candidate transform that sets KEY to VALUE."
  (lambda (object) (puthash key value object) object))

(ert-deftest org-glance-config-file-test-vectors ()
  (let ((path (getenv "GLANCE_CONFIG_VECTORS")))
    (unless path (ert-skip "Set GLANCE_CONFIG_VECTORS for shared compatibility vectors"))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (entry (append (json-parse-buffer) nil))
        (let* ((bytes (encode-coding-string (gethash "input" entry) 'utf-8-unix))
               (accepted (condition-case nil
                             (progn (org-glance-config-file:decode bytes) t)
                           (error nil))))
          (ert-info ((gethash "name" entry))
            (should (eq accepted (eq t (gethash "valid" entry))))))))))

(ert-deftest org-glance-config-file-test-invalid-utf8-and-depth ()
  (should-error (org-glance-config-file:decode
                 (concat "{\"format\":1,\"x\":\"" (unibyte-string 255) "\"}")))
  (should-error (org-glance-config-file:decode
                 (concat "{\"format\":1,\"x\":" (make-string 130 ?\[)
                         "0" (make-string 130 ?\]) "}"))))

(ert-deftest org-glance-config-file-test-unicode-write ()
  (org-glance-config-test--with-root
    (let ((snapshot (org-glance-config-file:write
                     root :change (org-glance-config-test--put "title" "文🙂 naïve"))))
      (should (equal "文🙂 naïve"
                     (gethash "title" (org-glance-config-file:object
                                        (org-glance-config-file:read root)))))
      (should (equal (org-glance-config-file:revision snapshot)
                     (org-glance-config-file:revision (org-glance-config-file:read root)))))))

(ert-deftest org-glance-config-file-test-missing-read-is-passive ()
  (org-glance-config-test--with-root
    (let ((snapshot (org-glance-config-file:read root)))
      (should-not (org-glance-config-file:revision snapshot))
      (should (= 1 (gethash "format" (org-glance-config-file:object snapshot))))
      (should (equal '("." "..") (directory-files root))))))

(ert-deftest org-glance-config-file-test-warm-read-work ()
  (org-glance-config-test--with-root
    (let* ((snapshot (org-glance-config-file:write
                      root :change (org-glance-config-test--put "title" "文🙂")))
           (path (org-glance-config-file:path root))
           (read-original (symbol-function 'insert-file-contents-literally))
           (reads 0))
      (cl-letf (((symbol-function 'directory-files)
                 (lambda (&rest _) (ert-fail "JSON read enumerated a directory")))
                ((symbol-function 'insert-file-contents-literally)
                 (lambda (file &rest args)
                   (should (equal path file))
                   (cl-incf reads)
                   (apply read-original file args))))
        (dotimes (_ 100)
          (should (equal (org-glance-config-file:revision snapshot)
                         (org-glance-config-file:revision
                          (org-glance-config-file:read root))))))
      (should (= reads 100)))))

(when (getenv "GLANCE_CONFIG_BENCH")
  (ert-deftest org-glance-config-file-test-read-baseline ()
    (org-glance-config-test--with-root
      (org-glance-config-file:write root :change #'identity)
      (copy-file (getenv "GLANCE_CONFIG_READ_FIXTURE") (org-glance-config-file:path root) t)
      (let* ((snapshot (org-glance-config-file:read root))
             (canonical (org-glance-config-file--serialize
                         (org-glance-config-file:object snapshot)))
             samples)
        (dotimes (_ 20) (org-glance-config-file:read root))
        (dotimes (_ 31)
          (let ((before (float-time)))
            (dotimes (_ 100)
              (let ((current (org-glance-config-file:read root)))
                (should (equal (org-glance-config-file:revision snapshot)
                               (org-glance-config-file:revision current)))
                (should (equal canonical
                               (org-glance-config-file--serialize
                                (org-glance-config-file:object current))))))
            (push (* 10 (- (float-time) before)) samples)))
        (setq samples (nreverse samples))
        (let ((ordered (sort (copy-sequence samples) #'<)))
          (princ (json-serialize
                  `((schema . "glance.config-file-read-baseline/1")
                    (runtime . "emacs")
                    (bytes . ,(file-attribute-size (file-attributes (org-glance-config-file:path root))))
                    (warmups . 20) (readsPerSample . 100)
                    (samplesMsPerRead . ,(vconcat samples))
                    (p50Ms . ,(nth 15 ordered)) (p95Ms . ,(nth 29 ordered))
                    (revision . ,(org-glance-config-file:revision snapshot)))))
          (terpri))))))

(ert-deftest org-glance-config-file-test-candidate-conflict-and-peer-preservation ()
  (org-glance-config-test--with-root
    (let* ((peer (make-hash-table :test 'equal))
           (first (org-glance-config-file:write
                   root :change (org-glance-config-test--put "clients" peer)))
           (second (org-glance-config-file:write
                    root :expected (org-glance-config-file:revision first)
                    :change (org-glance-config-test--put "actions" []))))
      (should (hash-table-p (gethash "clients" (org-glance-config-file:object second))))
      (should-error
       (org-glance-config-file:write
        root :expected (org-glance-config-file:revision first)
        :change (org-glance-config-test--put "tree" :null))
       :type 'org-glance-config-conflict)
      (should (equal (org-glance-config-file:revision second)
                     (org-glance-config-file:revision (org-glance-config-file:read root)))))))

(ert-deftest org-glance-config-file-test-noop-preserves-original-bytes ()
  (org-glance-config-test--with-root
    (org-glance-config-file:write root :change #'identity)
    (let ((path (org-glance-config-file:path root))
          (bytes " { \"format\" : 1 } \r\n"))
      (with-temp-file path (insert bytes))
      (let* ((snapshot (org-glance-config-file:read root))
             (revision (org-glance-config-file:revision snapshot))
             (after (org-glance-config-file:write root :expected revision :change #'identity)))
        (should (equal revision (org-glance-config-file:revision after)))
        (with-temp-buffer
          (insert-file-contents-literally path)
          (should (equal bytes (buffer-string))))
        (with-temp-file path (insert "{\"format\":1}\n"))
        (should-error
         (org-glance-config-file:write root :expected revision :change #'identity)
         :type 'org-glance-config-conflict)))))

(ert-deftest org-glance-config-file-test-reordered-objects-are-noop ()
  (org-glance-config-test--with-root
    (org-glance-config-file:write root :change #'identity)
    (let ((path (org-glance-config-file:path root))
          (bytes " { \"format\" : 1, \"n\" : 1.0, \"other\" : true, \"nested\" : {\"a\":null,\"b\":[{\"x\":false,\"y\":\"Title\"},{}]} }\r\n"))
      (with-temp-file path (insert bytes))
      (let* ((before (org-glance-config-file:read root))
             (revision (org-glance-config-file:revision before)))
        (dolist (mutate '(nil t))
          (cl-letf (((symbol-function 'org-glance-config-file--publish)
                     (lambda (&rest _) (ert-fail "Equivalent object was published"))))
            (let ((after (org-glance-config-file:write
                          root :expected revision
                          :change (lambda (object)
                                    (let ((candidate
                                           (json-parse-string
                                            "{\"nested\":{\"b\":[{\"y\":\"Title\",\"x\":false},{}],\"a\":null},\"other\":true,\"n\":1,\"format\":1.0}"
                                            :null-object :null :false-object :false)))
                                      (if mutate
                                          (progn
                                            (clrhash object)
                                            (maphash (lambda (key value) (puthash key value object)) candidate)
                                            object)
                                        candidate))))))
              (should (equal revision (org-glance-config-file:revision after)))
              (should (equal (org-glance-config-file--serialize
                              (org-glance-config-file:object before))
                             (org-glance-config-file--serialize
                              (org-glance-config-file:object after)))))))
        (with-temp-buffer
          (insert-file-contents-literally path)
          (should (equal bytes (buffer-string))))))))

(ert-deftest org-glance-config-file-test-semantic-changes-publish ()
  (dolist (change '(("{\"x\":null}" . "{}")
                    ("{\"x\":[]}" . "{\"x\":{}}")
                    ("{\"x\":null}" . "{\"x\":false}")
                    ("{\"x\":false}" . "{\"x\":0}")
                    ("{\"x\":\"Title\"}" . "{\"x\":\"title\"}")
                    ("{\"x\":[1,2]}" . "{\"x\":[2,1]}")
                    ("{\"x\":9007199254740993}" . "{\"x\":9007199254740992.0}")))
    (org-glance-config-test--with-root
      (let* ((parse (lambda (text)
                      (json-parse-string text :null-object :null :false-object :false)))
             (before (org-glance-config-file:write
                      root :change (org-glance-config-test--put "nested" (funcall parse (car change)))))
             (after (org-glance-config-file:write
                     root :expected (org-glance-config-file:revision before)
                     :change (org-glance-config-test--put "nested" (funcall parse (cdr change))))))
        (ert-info (change)
          (should-not (equal (org-glance-config-file:revision before)
                             (org-glance-config-file:revision after))))))))

(ert-deftest org-glance-config-file-test-nested-in-place-change ()
  (org-glance-config-test--with-root
    (let* ((before (org-glance-config-file:write
                    root :change (org-glance-config-test--put
                                  "nested" (json-parse-string "{\"x\":[\"Title\"]}"))))
           (after (org-glance-config-file:write
                   root :expected (org-glance-config-file:revision before)
                   :change (lambda (object)
                             (aset (aref (gethash "x" (gethash "nested" object)) 0) 0 ?t)
                             object))))
      (should-not (equal (org-glance-config-file:revision before)
                         (org-glance-config-file:revision after)))
      (should (equal "title" (aref (gethash "x" (gethash "nested"
                                     (org-glance-config-file:object
                                      (org-glance-config-file:read root)))) 0))))))

(ert-deftest org-glance-config-file-test-invalid-candidate-and-publication-failure ()
  (org-glance-config-test--with-root
    (let* ((snapshot (org-glance-config-file:write root :change #'identity))
           (revision (org-glance-config-file:revision snapshot)))
      (should-error (org-glance-config-file:write root :expected revision
                     :change (org-glance-config-test--put "format" 2)))
      (cl-letf (((symbol-function 'rename-file) (lambda (&rest _) (error "injected rename failure"))))
        (should-error (org-glance-config-file:write root :expected revision
                       :change (org-glance-config-test--put "tree" :null))))
      (should (equal revision (org-glance-config-file:revision (org-glance-config-file:read root))))
      (should (equal '("." ".." "config.json")
                     (directory-files (expand-file-name ".org-glance" root))))
      (org-glance-config-file:write root :expected revision :change #'identity))))

(ert-deftest org-glance-config-file-test-exceptions-and-retained-lock ()
  (org-glance-config-test--with-root
    (should-error (org-glance-config-file:with-lock root (lambda () (error "injected"))))
    (let ((lock (concat (org-glance-config-file:path root) ".lock")))
      (make-directory lock)
      (should-error (org-glance-config-file:write root :change #'identity)
                    :type 'org-glance-config-busy)
      (should (file-directory-p lock))
      (delete-directory lock)
      (org-glance-config-file:write root :change #'identity))))

(ert-deftest org-glance-config-file-test-malformed-disk-remains-visible ()
  (org-glance-config-test--with-root
    (org-glance-config-file:write root :change #'identity)
    (let ((path (org-glance-config-file:path root)))
      (with-temp-file path (insert "{bad"))
      (should-error (org-glance-config-file:read root))
      (should-error (org-glance-config-file:write root :change #'identity))
      (with-temp-buffer (insert-file-contents path) (should (equal "{bad" (buffer-string)))))
    (should-not (file-exists-p (concat (org-glance-config-file:path root) ".lock")))))

(ert-deftest org-glance-config-file-test-symlinks ()
  (org-glance-config-test--with-root
    (let ((tree (expand-file-name "tree" root))
          (outside (expand-file-name "outside" root)))
      (make-directory tree)
      (make-directory outside)
      (make-symbolic-link outside (expand-file-name ".org-glance" tree))
      (should-error (org-glance-config-file:read tree) :type 'org-glance-config-unsafe)
      (should-error (org-glance-config-file:write tree :change #'identity)
                    :type 'org-glance-config-unsafe)
      (should (equal '("." "..") (directory-files outside))))
    (let ((path (org-glance-config-file:path root))
          (outside (expand-file-name "target" root)))
      (make-directory (expand-file-name ".org-glance" root))
      (with-temp-file outside (insert "keep"))
      (make-symbolic-link outside path)
      (should-error (org-glance-config-file:write root :change #'identity)
                    :type 'org-glance-config-unsafe)
      (with-temp-buffer (insert-file-contents outside) (should (equal "keep" (buffer-string)))))))

(provide 'test-config-file)
;;; test-config-file.el ends here

(ert-deftest org-glance-config-file-test-observed-read-refuses-same-size-corruption ()
  (org-glance-config-test--with-root
    (let ((path (org-glance-config-file:path root)))
      (make-directory (file-name-directory path))
      (with-temp-file path (insert "{\"format\":1}"))
      (let ((observed (org-glance-config-file:read root)))
        (with-temp-file path (insert "{\"format\":2}"))
        (should-error (org-glance-config-file:read root observed)
                      :type 'org-glance-config-error)
        (should (= 1 (gethash "format" (org-glance-config-file:object observed))))
        (with-temp-file path (insert "{\"format\":1}"))
        (should (equal (org-glance-config-file:revision observed)
                       (org-glance-config-file:revision
                        (org-glance-config-file:read root observed))))))))

(ert-deftest org-glance-config-file-test-prepares-before-lock-and-rechecks-bytes ()
  (org-glance-config-test--with-root
    (let ((snapshot (org-glance-config-file:read root))
          (serialize (symbol-function 'org-glance-config-file--serialize))
          (decode (symbol-function 'org-glance-config-file:decode))
          (publish (symbol-function 'org-glance-config-file--publish))
          (lock (concat (org-glance-config-file:path root) ".lock"))
          (encodes 0) (publications 0))
      (cl-letf (((symbol-function 'org-glance-config-file--serialize)
                 (lambda (object)
                   (should-not (file-exists-p lock))
                   (cl-incf encodes)
                   (funcall serialize object)))
                ((symbol-function 'org-glance-config-file:decode)
                 (lambda (bytes)
                   (should-not (file-exists-p lock))
                   (funcall decode bytes)))
                ((symbol-function 'org-glance-config-file--publish)
                 (lambda (path bytes)
                   (should (file-directory-p lock))
                   (cl-incf publications)
                   (funcall publish path bytes))))
        (dolist (value '("first" "second" "second"))
          (setq snapshot
                (org-glance-config-file:write
                 root :expected (org-glance-config-file:revision snapshot)
                 :change (lambda (object)
                           (should-not (file-exists-p lock))
                           (puthash "value" value object)
                           object))))
        (should (= encodes 6))
        (should (= publications 2)))
      (should-not (file-exists-p lock)))))

(ert-deftest org-glance-config-file-test-peer-during-preparation-refuses-every-publication ()
  (dolist (present '(nil t))
    (dolist (changed '(nil t))
      (org-glance-config-test--with-root
        (when present (org-glance-config-file:write root :change #'identity))
        (let* ((before (org-glance-config-file:read root))
               (revision (org-glance-config-file:revision before))
               (calls 0) peer)
          (should-error
           (org-glance-config-file:write
            root :expected revision
            :change (lambda (object)
                      (cl-incf calls)
                      (setq peer (org-glance-config-file:write
                                  root :expected revision
                                  :change (org-glance-config-test--put "peer" t)))
                      (when changed (puthash "outer" t object))
                      object))
           :type 'org-glance-config-conflict)
          (should (= calls 1))
          (let ((after (org-glance-config-file:read root)))
            (should (equal (org-glance-config-file:revision peer)
                           (org-glance-config-file:revision after)))
            (should (eq t (gethash "peer" (org-glance-config-file:object after))))
            (should-not (gethash "outer" (org-glance-config-file:object after))))
          (should-not (file-exists-p (concat (org-glance-config-file:path root)
                                            ".lock"))))))))

(ert-deftest org-glance-config-file-test-malformed-peer-keeps-syntax-precedence ()
  (org-glance-config-test--with-root
    (let* ((before (org-glance-config-file:write root :change #'identity))
           (path (org-glance-config-file:path root)))
      (should
       (eq 'org-glance-config-error
           (car (should-error
                 (org-glance-config-file:write
                  root :expected (org-glance-config-file:revision before)
                  :change (lambda (object)
                            (with-temp-file path (insert "{bad"))
                            object))))))
      (with-temp-buffer
        (insert-file-contents-literally path)
        (should (equal "{bad" (buffer-string))))
      (should-not (file-exists-p (concat path ".lock"))))))

(ert-deftest org-glance-config-file-test-deletion-during-preparation-refuses-publication ()
  (dolist (changed '(nil t))
    (org-glance-config-test--with-root
      (let* ((before (org-glance-config-file:write root :change #'identity))
             (path (org-glance-config-file:path root)))
        (should-error
         (org-glance-config-file:write
          root :expected (org-glance-config-file:revision before)
          :change (lambda (object)
                    (delete-file path)
                    (when changed (puthash "outer" t object))
                    object))
         :type 'org-glance-config-conflict)
        (should-not (file-exists-p path))
        (should-not (file-exists-p (concat path ".lock")))))))

(ert-deftest org-glance-config-file-test-known-refusal-precedes-callback ()
  (org-glance-config-test--with-root
    (org-glance-config-file:write root :change #'identity)
    (let* ((lock (concat (org-glance-config-file:path root) ".lock"))
           (change (lambda (_) (ert-fail "Refused write ran its callback"))))
      (should-error (org-glance-config-file:write root :change change)
                    :type 'org-glance-config-conflict)
      (make-symbolic-link "absent-lock-target" lock)
      (should-error (org-glance-config-file:write root :change change)
                    :type 'org-glance-config-busy)
      (should (file-symlink-p lock))
      (delete-file lock))))

(ert-deftest org-glance-config-file-test-path-replacement-after-preparation-is-refused ()
  (org-glance-config-test--with-root
    (let* ((before (org-glance-config-file:write root :change #'identity))
           (path (org-glance-config-file:path root))
           (target (expand-file-name "untouched" root)))
      (with-temp-file target (insert "keep"))
      (should-error
       (org-glance-config-file:write
        root :expected (org-glance-config-file:revision before)
        :change (lambda (object)
                  (delete-file path)
                  (make-symbolic-link target path)
                  object))
       :type 'org-glance-config-unsafe)
      (should (file-symlink-p path))
      (with-temp-buffer (insert-file-contents-literally target)
                        (should (equal "keep" (buffer-string))))
      (should-not (file-exists-p (concat path ".lock"))))))

(ert-deftest org-glance-config-file-test-late-lock-owner-is-retained ()
  (org-glance-config-test--with-root
    (let* ((before (org-glance-config-file:write root :change #'identity))
           (revision (org-glance-config-file:revision before))
           (lock (concat (org-glance-config-file:path root) ".lock")))
      (should-error
       (org-glance-config-file:write
        root :expected revision
        :change (lambda (object) (make-directory lock) object))
       :type 'org-glance-config-busy)
      (should (file-directory-p lock))
      (should (equal revision (org-glance-config-file:revision
                               (org-glance-config-file:read root))))
      (delete-directory lock))))

(ert-deftest org-glance-config-file-test-quit-cleans-preparation-and-publication ()
  (dolist (phase '(prepare publish))
    (org-glance-config-test--with-root
      (let* ((before (org-glance-config-file:write root :change #'identity))
             (revision (org-glance-config-file:revision before))
             (write (symbol-function 'write-region)))
        (should
         (eq 'quit
             (condition-case nil
                 (cl-letf (((symbol-function 'write-region)
                            (lambda (&rest args)
                              (if (eq phase 'publish) (signal 'quit nil)
                                (apply write args)))))
                   (org-glance-config-file:write
                    root :expected revision
                    :change (lambda (object)
                              (when (eq phase 'prepare) (signal 'quit nil))
                              (puthash "changed" t object) object)))
               (quit 'quit))))
        (should (equal revision (org-glance-config-file:revision
                                 (org-glance-config-file:read root))))
        (should (equal '("." ".." "config.json")
                       (directory-files (expand-file-name ".org-glance" root))))))))
