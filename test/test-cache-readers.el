;;; test-cache-readers.el --- Read connection lifecycle -*- lexical-binding: t; -*-

(require 'test-helpers)

(ert-deftest org-glance-test:cache-reader-reuses-one-open-for-warm-reads ()
  "One hundred reads share one connection and end every transaction."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:close-readers graph)
    (let ((opens 0) (open (symbol-function 'sqlite-open)) connection)
      (cl-letf (((symbol-function 'sqlite-open)
                 (lambda (&rest args) (cl-incf opens) (apply open args))))
        (dotimes (_ 100)
          (org-glance-cache--read-db
           graph (lambda (db)
                   (if connection (should (eq connection db)) (setq connection db))
                   (should (equal '((1)) (sqlite-select db "PRAGMA query_only"))))))
        (should (= opens 1)))
      (sqlite-execute connection "BEGIN")
      (sqlite-commit connection)
      (org-glance-cache:close-readers graph)
      (should-error (sqlite-select connection "SELECT 1") :type 'error))))

(ert-deftest org-glance-test:cache-reader-sees-external-commits-in-fresh-snapshots ()
  "A reused reader releases its snapshot and sees another connection's commit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((writer (sqlite-open (org-glance-cache:path graph))) reader)
      (unwind-protect
          (progn
            (should (equal '(("Alpha"))
                           (org-glance-cache--read-db
                            graph (lambda (db)
                                    (setq reader db)
                                    (sqlite-select db "SELECT title FROM headline")))))
            (sqlite-execute writer "UPDATE headline SET title='Peer'")
            (should (equal '(("Peer"))
                           (org-glance-cache--read-db
                            graph (lambda (db)
                                    (should (eq reader db))
                                    (sqlite-select db "SELECT title FROM headline"))))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:cache-reader-errors-and-quits-release-leases ()
  "An aborted callback retires its connection and the next read succeeds."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (dolist (kind '(error quit org-glance-cache-reader-replaced))
      (let (failed (calls 0))
        (condition-case err
            (org-glance-cache--read-db
             graph (lambda (db) (setq failed db) (cl-incf calls)
                     (signal kind '("abort"))))
          ((error quit) (should (eq kind (car err)))))
        (should (= 1 calls))
        (should-error (sqlite-select failed "SELECT 1") :type 'error)
        (should (equal '((1))
                       (org-glance-cache--read-db
                        graph (lambda (db)
                                (should-not (eq db failed))
                                (sqlite-select db "SELECT 1")))))))))

(ert-deftest org-glance-test:cache-reader-nested-reads-preserve-the-outer-snapshot ()
  "A nested read uses a separate connection and leaves its parent pinned."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((writer (sqlite-open (org-glance-cache:path graph))) nested outer)
      (unwind-protect
          (progn
            (org-glance-cache--read-db
             graph (lambda (db)
                     (setq outer db)
                     (should (equal '(("Alpha")) (sqlite-select db "SELECT title FROM headline")))
                     (sqlite-execute writer "UPDATE headline SET title='Peer'")
                     (org-glance-cache--read-db
                      graph (lambda (other)
                              (setq nested other)
                              (should-not (eq db other))
                              (should (equal '(("Peer")) (sqlite-select other "SELECT title FROM headline")))))
                     (should (equal '(("Alpha")) (sqlite-select db "SELECT title FROM headline")))))
            (should-error (sqlite-select nested "SELECT 1") :type 'error)
            (org-glance-cache--read-db
             graph (lambda (db)
                     (should (eq db outer))
                     (should (equal '(("Peer")) (sqlite-select db "SELECT title FROM headline"))))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:cache-reader-reopens-replaced-and-missing-files ()
  "Reopen replacements and repair missing caches before public point reads."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let* ((path (org-glance-cache:path graph))
           (replacement (concat path ".replacement"))
           (old (org-glance-cache--read-db graph #'identity)))
      (let ((writer (sqlite-open path)))
        (unwind-protect
            (sqlite-execute writer "VACUUM INTO ?" (vector replacement))
          (sqlite-close writer)))
      (org-glance-cache:close-readers graph)
      (dolist (suffix '("-wal" "-shm"))
        (ignore-errors (delete-file (concat path suffix))))
      (setq old (org-glance-cache--read-db graph #'identity))
      (should (= 1 (length (org-glance-cache:headlines graph))))
      (let ((writer (sqlite-open replacement)))
        (unwind-protect
            (sqlite-execute writer "UPDATE headline_family SET state='deleted'")
          (sqlite-close writer)))
      (rename-file replacement path t)
      (should (equal '(("Alpha"))
                     (org-glance-cache--read-db
                      graph (lambda (db)
                              (should-not (eq db old))
                              (should-not (org-glance-cache--read-projection db))
                              (sqlite-select db "SELECT title FROM headline")))))
      (should-error (sqlite-select old "SELECT 1") :type 'error)
      (org-glance-cache:close-readers graph)
      (delete-file path)
      (should-not (org-glance-cache--read-db graph #'identity t))
      (should-not (file-exists-p path))
      (should (equal "Alpha" (org-glance-headline-metadata:title
                               (org-glance-cache:headline graph "a"))))
      (should (file-exists-p path)))))

(ert-deftest org-glance-test:cache-reader-retirement-defers-active-close ()
  "Explicit retirement preserves an active transaction until it returns."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((reader (org-glance-cache--read-db
                   graph (lambda (db)
                           (org-glance-cache:close-readers graph)
                           (should (equal '((1)) (sqlite-select db "SELECT 1")))
                           db))))
      (should-error (sqlite-select reader "SELECT 1") :type 'error)
      (should (equal '((1)) (org-glance-cache--read-db
                             graph (lambda (db) (sqlite-select db "SELECT 1"))))))))

(ert-deftest org-glance-test:cache-reader-lru-evicts-idle-connections ()
  "The reader limit evicts the least recently used idle graph."
  (let ((org-glance-cache--readers nil)
        (org-glance-cache--reader-limit 2))
    (org-glance-test:with-graph first
      (org-glance-test:with-graph second
        (org-glance-test:with-graph third
          (dolist (graph (list first second third))
            (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha")))
          (org-glance-cache:close-readers)
          (let ((one (org-glance-cache--read-db first #'identity))
                (two (org-glance-cache--read-db second #'identity)))
            (org-glance-cache--read-db first #'identity)
            (org-glance-cache--read-db third #'identity)
            (should (= 2 (length org-glance-cache--readers)))
            (should (equal '((1)) (sqlite-select one "SELECT 1")))
            (should-error (sqlite-select two "SELECT 1") :type 'error)))))))

(ert-deftest org-glance-test:cache-reader-staging-connections-are-private ()
  "A staging read neither borrows nor retains the ordinary graph reader."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let* ((live (org-glance-cache--read-db graph #'identity))
           (staged (let ((org-glance-cache--staging-path (org-glance-cache:path graph)))
                     (org-glance-cache--read-db graph #'identity))))
      (should-not (eq live staged))
      (should-error (sqlite-select staged "SELECT 1") :type 'error)
      (should (eq live (org-glance-cache--read-db graph #'identity))))))

(ert-deftest org-glance-test:cache-reader-replacement-during-pin-retries-before-callback ()
  "Replacement during snapshot admission retries without replaying the callback."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let* ((path (org-glance-cache:path graph))
           (replacement (concat path ".replacement"))
           (writer (sqlite-open path))
           (identity (symbol-function 'org-glance-cache--file-identity))
           (checks 0) (calls 0))
      (unwind-protect
          (sqlite-execute writer "VACUUM INTO ?" (vector replacement))
        (sqlite-close writer))
      (org-glance-cache:close-readers graph)
      (cl-letf (((symbol-function 'org-glance-cache--file-identity)
                 (lambda (candidate)
                   (when (= 2 (cl-incf checks))
                     (rename-file replacement path t))
                   (funcall identity candidate))))
        (should (equal '(("Alpha"))
                       (org-glance-cache--read-db
                        graph (lambda (db)
                                (cl-incf calls)
                                (sqlite-select db "SELECT title FROM headline"))))))
      (should (= 1 calls))
      (should (= 4 checks)))))

(ert-deftest org-glance-test:cache-reader-empty-wal-preserves-overview-receipt ()
  "An empty read-created WAL preserves the receipt; a committed frame changes it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:close-readers graph)
    (let* ((before (org-glance-overview--dependency graph nil))
           (path (org-glance-cache:path graph))
           (wal (concat path "-wal")))
      (org-glance-cache--read-db graph #'identity)
      (should (file-exists-p wal))
      (should (zerop (file-attribute-size (file-attributes wal))))
      (should (equal before (org-glance-overview--dependency graph nil)))
      (let ((writer (sqlite-open path)))
        (unwind-protect
            (sqlite-execute writer "UPDATE headline SET title='Peer'")
          (sqlite-close writer)))
      (should (> (file-attribute-size (file-attributes wal)) 0))
      (should-not (equal before (org-glance-overview--dependency graph nil))))))

(provide 'test-cache-readers)
;;; test-cache-readers.el ends here

(ert-deftest org-glance-test:picker-projection-reuses-unchanged-snapshots ()
  "Warm pickers avoid scans and decoding and refresh after peer commits."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* Alpha :work:")
      (org-glance-test:headline "b" "* Beta :home:"))
    (org-glance-cache:headlines graph)
    (let ((compute (symbol-function 'org-glance-cache--compute-projection))
          (decode (symbol-function 'org-glance-headline-metadata:deserialize))
          (scans 0) (decodes 0))
      (cl-letf (((symbol-function 'org-glance-cache--compute-projection)
                 (lambda (db) (cl-incf scans) (funcall compute db)))
                ((symbol-function 'org-glance-headline-metadata:deserialize)
                 (lambda (record) (cl-incf decodes) (funcall decode record))))
        (let ((rows (org-glance-cache:headlines graph)))
          (setcdr rows nil)
          (should (= 2 (length (org-glance-cache:headlines graph)))))
        (dotimes (_ 10)
          (should (= 2 (length (org-glance-material:candidates graph))))
          (let ((org-glance-graph graph))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt tags &rest _)
                         (should (equal '("home" "work") tags)) "work")))
              (should (equal "work" (org-glance-view:completing-read-tag))))))
        (should (= 0 scans decodes))
        (let ((writer (sqlite-open (org-glance-cache:path graph))))
          (unwind-protect
              (sqlite-execute writer
                              "UPDATE headline_family SET state='deleted' WHERE logical_id='a'")
            (sqlite-close writer)))
        (should (equal '("b") (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-cache:headlines graph))))
        (should (= 1 scans))
        (org-glance-cache:headlines graph)
        (should (= 1 scans))))))

(ert-deftest org-glance-test:picker-projection-keeps-pinned-snapshots ()
  "A peer commit during a read invalidates reuse on the following snapshot."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:headlines graph)
    (let ((writer (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (org-glance-cache--read-db
             graph (lambda (db)
                     (sqlite-execute writer
                                     "UPDATE headline_family SET state='deleted' WHERE logical_id='a'")
                     (should (= 1 (length (org-glance-cache--read-projection db))))))
            (org-glance-cache--read-db
             graph (lambda (db)
                     (should-not (org-glance-cache--read-projection db)))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:overview-selection-restores-gc-before-prompt ()
  "Overview prepares tags with bounded allocation and restores policy at input."
  (dolist (threshold (list 800000 (* 128 1024 1024)))
    (let ((gc-cons-threshold threshold)
          (org-glance-graph (make-org-glance-graph)))
      (cl-letf (((symbol-function 'org-glance-cache:headlines)
                 (lambda (_graph)
                   (should (= gc-cons-threshold
                              (max threshold (* 64 1024 1024))))
                   (list (make-org-glance-headline-metadata :tags '("work")))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt tags &rest _)
                   (should (= threshold gc-cons-threshold))
                   (car tags))))
        (should (equal "work" (org-glance-view:completing-read-tag)))))))
