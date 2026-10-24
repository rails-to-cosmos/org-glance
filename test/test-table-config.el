;;; test-table-config.el --- Shared JSON layout contracts -*- lexical-binding: t -*-

(require 'test-helpers)
(require 'org-glance-table-config)

(defun org-glance-table-config-test--publish (graph text)
  "Publish JSON TEXT and observe its settings in GRAPH."
  (org-glance-test:write
   (org-glance-config-file:path (org-glance-graph:directory graph)) text)
  (org-glance-config:refresh graph))

(defun org-glance-table-config-test--bytes (graph)
  "Return GRAPH's exact shared configuration bytes."
  (with-temp-buffer
    (insert-file-contents-literally
     (org-glance-config-file:path (org-glance-graph:directory graph)))
    (buffer-string)))

(ert-deftest org-glance-table-config-test-observed-absence-creates-only-json ()
  (org-glance-test:with-graph graph
    (let* ((root (org-glance-graph:directory graph))
           (snapshot (org-glance-config-file:read root))
           (edits '(("tableColumns" "book" (:columns (("AUTHOR" . "Writer")))))))
      (should-not (org-glance-config-file:revision snapshot))
      (should-error (org-glance-table-config:write root :edits edits) :type 'user-error)
      (should-not (file-exists-p (org-glance-config-file:path root)))
      (let ((saved (org-glance-table-config:write root :snapshot snapshot :edits edits)))
        (should (stringp (org-glance-config-file:revision saved)))
        (should (equal '(:columns (("AUTHOR" . "Writer")))
                       (cdr (assoc "book" (org-glance-table-config:entries saved :section "tableColumns"))))))
      (let ((bytes (org-glance-table-config-test--bytes graph)))
        (should-error (org-glance-table-config:write root :snapshot snapshot :edits edits)
                      :type 'org-glance-config-conflict)
        (should (equal bytes (org-glance-table-config-test--bytes graph))))
      (should-not (file-exists-p (org-glance-table--schema-file graph))))))

(ert-deftest org-glance-table-config-test-json-layout-is-atomic-and-preserves-peers ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish
     graph "{\"format\":1,\"clients\":{\"glance\":{\"theme\":\"dark\"},\"orgGlance\":{\"tableColumns\":{\"book\":{\"columns\":[{\"property\":\"AUTHOR\",\"header\":\"Writer\"}],\"hidden\":[\"tags\"],\"extension\":true}}}}}")
    (org-glance--eld-alist-set (org-glance-table--schema-file graph) "book"
                               '(:columns (("WRONG" . "Wrong"))))
    (let ((legacy (with-temp-buffer
                    (insert-file-contents-literally (org-glance-table--schema-file graph))
                    (buffer-string))))
      (org-glance-graph:add
       graph (org-glance-test:headline-props
              "b1" "* TODO Book :book:" '(("AUTHOR" . "One") ("EXPECTED" . "42"))))
      (org-glance-test:with-table (graph 'book)
        (should (member "AUTHOR" (org-glance-test:table-col-keys)))
        (should-not (member "WRONG" (org-glance-test:table-col-keys)))
        (should-not (member "tags" (org-glance-test:table-col-keys)))
        (let ((publish (symbol-function 'org-glance-config-file--publish)) (writes 0))
          (cl-letf (((symbol-function 'org-glance-config-file--publish)
                     (lambda (&rest args) (cl-incf writes) (apply publish args))))
            (table-view-add-column
             (org-glance-table--property-column graph "EXPECTED" "Forecast 📈")))
          (should (= writes 1)))
        (should (equal org-glance-table--config-snapshot
                       (org-glance-table--current-config))))
      (let* ((snapshot (org-glance-config-file:read (org-glance-graph:directory graph)))
             (object (org-glance-config-file:object snapshot))
             (clients (gethash "clients" object))
             (native (gethash "orgGlance" clients))
             (schema (gethash "book" (gethash "tableColumns" native))))
        (should (equal "dark" (gethash "theme" (gethash "glance" clients))))
        (should (eq t (gethash "extension" schema)))
        (should (hash-table-p (gethash "tableViews" native))))
      (should (equal legacy (with-temp-buffer
                              (insert-file-contents-literally (org-glance-table--schema-file graph))
                              (buffer-string))))
      (should-not (file-exists-p (org-glance-table--config-file graph)))
      (should-not (file-exists-p (org-glance-table--refs-file graph)))
      (org-glance-test:with-table (graph 'book)
        (should (equal '("AUTHOR" "EXPECTED")
                       (mapcar #'car (plist-get (org-glance-table--schema-entry graph 'book)
                                               :columns))))
        (should (equal "Forecast 📈"
                       (alist-get 'header (cl-find "EXPECTED"
                                                   (table-view--columns table-view--spec)
                                                   :key (lambda (c) (alist-get 'key c))
                                                   :test #'equal))))))))

(ert-deftest org-glance-table-config-test-json-reference-layout-preserves-sort-and-scope ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (org-glance-test:ref-fixture graph)
    (let ((before (org-glance-table-config-test--bytes graph)))
      (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
        (table-view-remove-column "tags")
        (table-view-set-sort '(("title" . desc-nulls-first)))
        (org-glance-table--persist-config)
        (should (equal before (org-glance-table-config-test--bytes graph)))
        (org-glance-test:offering (choices (car choices))
          (org-glance-table:apply-layout))))
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should-not (member "tags" (org-glance-test:table-col-keys)))
      (should (equal '(("title" . desc-nulls-first)) table-view--sort-keys)))
    (org-glance-test:with-table (graph '(:id-any ("r2")) '(:anchor "c2" :dir relations))
      (should (member "tags" (org-glance-test:table-col-keys))))
    (should-not (file-exists-p (org-glance-table--refs-file graph)))))

(ert-deftest org-glance-table-config-test-stale-view-preserves-layout-and-peer-write ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Book :book:"))
    (org-glance-test:with-table (graph 'book)
      (let* ((old org-glance-table--settings-source)
             (snapshot (cdr old))
             (peer (org-glance-config-file:write
                    (org-glance-graph:directory graph)
                    :expected (org-glance-config-file:revision snapshot)
                    :change (lambda (object) (puthash "peer" t object) object)))
             (bytes (org-glance-table-config-test--bytes graph)))
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))
        (should (member "AUTHOR" (org-glance-test:table-col-keys)))
        (should-error (org-glance-table:apply-layout) :type 'org-glance-config-conflict)
        (should (eq old org-glance-table--settings-source))
        (should-not (equal org-glance-table--config-snapshot (org-glance-table--current-config)))
        (should (equal bytes (org-glance-table-config-test--bytes graph)))
        (should-not (equal (org-glance-config-file:revision peer)
                           (org-glance-config-file:revision snapshot)))))))

(ert-deftest org-glance-table-config-test-legacy-view-refuses-a-new-json-authority ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Book :book:"))
    (org-glance-test:with-table (graph 'book)
      (org-glance-table-config-test--publish graph "{\"format\":1}")
      (let ((bytes (org-glance-table-config-test--bytes graph)))
        (should-error (org-glance-table--config-put
                       graph 'book (org-glance-table--current-config))
                      :type 'org-glance-config-conflict)
        (should-not (file-exists-p (org-glance-table--config-file graph)))
        (should (equal bytes (org-glance-table-config-test--bytes graph)))))))

(ert-deftest org-glance-table-config-test-failed-write-retains-the-save-baseline ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Book :book:"))
    (org-glance-test:with-table (graph 'book)
      (let ((before (copy-tree org-glance-table--config-snapshot))
            (lock (concat (org-glance-config-file:path (org-glance-graph:directory graph)) ".lock")))
        (table-view-set-sort '(("title" . t)))
        (make-directory lock)
        (unwind-protect (org-glance-table--persist-config) (delete-directory lock))
        (should (equal before org-glance-table--config-snapshot))
        (org-glance-table--persist-config)
        (should (equal org-glance-table--config-snapshot (org-glance-table--current-config)))
        (should-not (file-exists-p (org-glance-table--config-file graph)))))))

(ert-deftest org-glance-table-config-test-invalid-multi-edit-publishes-nothing ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (let ((before (org-glance-table-config-test--bytes graph))
          (snapshot (org-glance-config:snapshot graph)))
      (should-error
       (org-glance-table-config:write
        (org-glance-graph:directory graph) :snapshot snapshot
        :edits '(("tableViews" "book" (:columns ("title") :sort nil))
                 ("tableColumns" "book" (:unsupported "bad"))))
       :type 'org-glance-config-error)
      (should (equal before (org-glance-table-config-test--bytes graph)))
      (should-not (file-exists-p (concat (org-glance-config-file:path
                                        (org-glance-graph:directory graph)) ".lock"))))))

(ert-deftest org-glance-table-config-test-no-op-and-reset-keep-exact-bytes ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1,\"clients\":{\"glance\":{\"keep\":true}}}")
    (let* ((root (org-glance-graph:directory graph))
           (first (org-glance-table-config:write
                   root :snapshot (org-glance-config:snapshot graph)
                   :edits '(("tableViews" "book" (:columns ("title") :sort (("title" . t)))))))
           (before (org-glance-table-config-test--bytes graph))
           (writes 0))
      (cl-letf (((symbol-function 'org-glance-config-file--publish)
                 (lambda (&rest _) (cl-incf writes))))
        (org-glance-table-config:write
         root :snapshot first
         :edits '(("tableViews" "book" (:columns ("title") :sort (("title" . t))))
                  ("tableRefs" "missing" nil))))
      (should (= writes 0))
      (should (equal before (org-glance-table-config-test--bytes graph))))))

(ert-deftest org-glance-table-config-test-wire-vector-and-unsupported-native-values ()
  (let* ((encoded (org-glance-table-config:encode
                   "tableRefs" '(:columns (("AUTHOR" . "Writer 🖋")) :hidden ("tags")
                                 :order ("title" "AUTHOR") :sort (("AUTHOR" . asc-nulls-first))
                                 :applied 17)))
         (sort (aref (gethash "sort" encoded) 0)))
    (should (equal "Writer 🖋" (gethash "header" (aref (gethash "columns" encoded) 0))))
    (should (equal '("AUTHOR" "asc" "first")
                   (mapcar (lambda (key) (gethash key sort)) '("column" "direction" "nulls"))))
    (should (= 17 (gethash "applied" encoded))))
  (should-error (org-glance-table-config:encode "tableViews" '(:sort (("title" . unknown))))
                :type 'org-glance-config-error)
  (let ((cycle (list :columns nil)))
    (setcdr (cdr cycle) cycle)
    (should-error (org-glance-table-config:encode "tableViews" cycle)
                  :type 'org-glance-config-error)))

(ert-deftest org-glance-table-config-test-explicit-apply-reports-a-busy-writer ()
  (dolist (json '(nil t))
    (org-glance-test:with-graph graph
      (when json (org-glance-table-config-test--publish graph "{\"format\":1}"))
      (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Book :book:"))
      (org-glance-test:with-table (graph 'book)
        (let ((before (copy-tree org-glance-table--config-snapshot))
              (lock (concat (org-glance-config-file:path (org-glance-graph:directory graph)) ".lock")))
          (table-view-set-sort '(("title" . t)))
          (make-directory lock)
          (unwind-protect
              (should-error (org-glance-table:apply-layout) :type 'org-glance-config-busy)
            (delete-directory lock))
          (should (equal before org-glance-table--config-snapshot)))))))

(ert-deftest org-glance-table-config-test-invalid-json-never-reads-legacy-layouts ()
  (org-glance-test:with-graph graph
    (org-glance--eld-alist-set (org-glance-table--config-file graph)
                               (org-glance-filter:identity nil) '(:columns ("WRONG")))
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (org-glance-test:write
     (org-glance-config-file:path (org-glance-graph:directory graph))
     "{\"format\":1,\"clients\":{\"orgGlance\":{\"tableViews\":[]}}}")
    (org-glance-config:refresh graph)
    (should-error (org-glance-table--config-get graph nil) :type 'org-glance-config-error)))

(ert-deftest org-glance-table-config-test-nested-extensions-survive-no-op-and-identity-edits ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish
     graph "{\"format\":1,\"clients\":{\"orgGlance\":{\"tableRefs\":{\"scope\":{\"columns\":[{\"property\":\"A\",\"header\":\"First\",\"future\":{\"value\":1}},{\"property\":\"B\",\"header\":\"Second\",\"future\":2},{\"property\":\"D\",\"header\":\"Removed\",\"future\":4}],\"sort\":[{\"column\":\"A\",\"direction\":\"asc\",\"future\":1},{\"column\":\"B\",\"direction\":\"desc\",\"nulls\":\"first\",\"future\":2},{\"column\":\"D\",\"direction\":\"asc\",\"future\":4}],\"outer\":true}}}}}")
    (let* ((root (org-glance-graph:directory graph))
           (snapshot (org-glance-config:snapshot graph))
           (entry (cdr (assoc "scope" (org-glance-table-config:entries snapshot :section "tableRefs"))))
           (before (org-glance-table-config-test--bytes graph))
           (writes 0))
      (cl-letf (((symbol-function 'org-glance-config-file--publish)
                 (lambda (&rest _) (cl-incf writes))))
        (org-glance-table-config:write root :snapshot snapshot
                                      :edits (list (list "tableRefs" "scope" entry))))
      (should (= 0 writes))
      (should (equal before (org-glance-table-config-test--bytes graph)))
      (let* ((saved (org-glance-table-config:write
                     root :snapshot snapshot
                     :edits '(("tableRefs" "scope"
                               (:columns (("B" . "Renamed") ("A" . "First") ("C" . "New"))
                                :sort (("B" . t) ("A") ("C" . asc-nulls-first)))))))
             (record (gethash "scope"
                              (gethash "tableRefs" (gethash "orgGlance"
                                                          (gethash "clients" (org-glance-config-file:object saved)))))))
        (should (eq t (gethash "outer" record)))
        (dolist (field '("columns" "sort"))
          (let ((items (gethash field record))
                (key (if (equal field "columns") "property" "column")))
            (should (equal '("B" "A" "C") (mapcar (lambda (item) (gethash key item)) (append items nil))))
            (should (= 2 (gethash "future" (aref items 0))))
            (should (equal (if (equal field "columns")
                               (gethash "value" (gethash "future" (aref items 1)))
                             (gethash "future" (aref items 1))) 1))
            (should-not (gethash "future" (aref items 2)))))
        (should (equal "Renamed" (gethash "header" (aref (gethash "columns" record) 0))))
        (should (equal "last" (gethash "nulls" (aref (gethash "sort" record) 0))))
        (should-not (gethash "nulls" (aref (gethash "sort" record) 1)))))))

(ert-deftest org-glance-table-config-test-ambiguous-nested-identities-publish-nothing ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (let ((before (org-glance-table-config-test--bytes graph)))
      (dolist (edit '(("tableColumns" "book" (:columns (("A" . "One") ("A" . "Two"))))
                      ("tableViews" "book" (:sort (("title" . t) ("title"))))))
        (should-error (org-glance-table-config:write
                       (org-glance-graph:directory graph) :snapshot (org-glance-config:snapshot graph)
                       :edits (list edit))
                      :type 'org-glance-config-error)
        (should (equal before (org-glance-table-config-test--bytes graph)))))
    (org-glance-table-config-test--publish
     graph "{\"format\":1,\"clients\":{\"orgGlance\":{\"tableColumns\":{\"book\":{\"columns\":[{\"property\":\"A\",\"header\":\"One\",\"future\":1},{\"property\":\"A\",\"header\":\"Two\",\"future\":2}]}}}}}")
    (let ((before (org-glance-table-config-test--bytes graph)))
      (should-error (org-glance-table-config:write
                     (org-glance-graph:directory graph) :snapshot (org-glance-config:snapshot graph)
                     :edits '(("tableColumns" "book" (:columns (("A" . "One"))))))
                    :type 'org-glance-config-error)
      (should (equal before (org-glance-table-config-test--bytes graph))))))

(ert-deftest org-glance-table-config-test-prepared-refresh-reuses-validation ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (let* ((org-glance-table--settings-source
            (cons graph (org-glance-config:snapshot graph)))
           (org-glance-table--reading-source nil)
           (decode (symbol-function 'org-glance-config:decode))
           (strict (symbol-function 'org-glance-config-file:decode))
           (fingerprints (symbol-function 'org-glance-config:fingerprints))
           (key (org-glance-config:cache-key graph))
           (presentation (org-glance-config:presentation-key graph))
           (typed-count 0) (strict-count 0) (fingerprint-count 0)
           (edits '(("tableViews" "book" (:columns ("title"))))))
      (cl-letf (((symbol-function 'org-glance-config:decode)
                 (lambda (&rest args) (cl-incf typed-count) (apply decode args)))
                ((symbol-function 'org-glance-config-file:decode)
                 (lambda (&rest args) (cl-incf strict-count) (apply strict args)))
                ((symbol-function 'org-glance-config:fingerprints)
                 (lambda (&rest args) (cl-incf fingerprint-count) (apply fingerprints args))))
        (org-glance-table--write-json-layout graph edits)
        (should (equal '(1 2 1) (list typed-count strict-count fingerprint-count)))
        (should (equal key (org-glance-config:cache-key graph)))
        (should (equal presentation (org-glance-config:presentation-key graph)))
        (setq typed-count 0 strict-count 0 fingerprint-count 0)
        (let ((bytes (org-glance-table-config-test--bytes graph)))
          (org-glance-table--write-json-layout graph edits)
          (should (equal '(1 1 0) (list typed-count strict-count fingerprint-count)))
          (should (equal bytes (org-glance-table-config-test--bytes graph))))))))

(ert-deftest org-glance-table-config-test-delayed-receipt-observes-newer-bytes ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (let* ((root (org-glance-graph:directory graph))
           (key (org-glance-config:cache-key graph))
           (receipt (org-glance-table-config--write
                     root (org-glance-config:snapshot graph)
                     '(("tableViews" "book" (:columns ("title")))))))
      (org-glance-test:write
       (org-glance-config-file:path root)
       "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}],\"future\":true}")
      (let ((peer (org-glance-config-file:read root)))
        (org-glance-config:refresh graph receipt)
        (should (equal (org-glance-config-file:revision peer)
                       (org-glance-config-file:revision (org-glance-config:snapshot graph))))
        (should-not (equal key (org-glance-config:cache-key graph)))
        (org-glance-config:refresh graph receipt)
        (should (equal (org-glance-config-file:revision peer)
                       (org-glance-config-file:revision (org-glance-config:snapshot graph))))
        (should (eq t (gethash "future" (org-glance-config-file:object
                                       (org-glance-config:snapshot graph)))))))))

(ert-deftest org-glance-table-config-test-preparation-failure-retains-saved-receipt ()
  (org-glance-test:with-graph graph
    (org-glance-table-config-test--publish graph "{\"format\":1}")
    (let* ((initial (org-glance-config:snapshot graph))
           (org-glance-table--settings-source (cons graph initial))
           (org-glance-table--reading-source nil)
           (edits '(("tableViews" "book" (:columns ("title"))))))
      (cl-letf (((symbol-function 'org-glance-config:fingerprints)
                 (lambda (&rest _) (error "synthetic preparation failure")))
                ((symbol-function 'display-warning) #'ignore))
        (let ((err (should-error (org-glance-table--write-json-layout graph edits)
                                 :type 'user-error)))
          (should (string-search "JSON saved at" (error-message-string err)))))
      (let* ((saved (cdr org-glance-table--settings-source))
             (revision (org-glance-config-file:revision saved))
             (bytes (org-glance-table-config-test--bytes graph)))
        (should-not (equal revision (org-glance-config-file:revision initial)))
        (should (equal (org-glance-config-file:revision initial)
                       (org-glance-config-file:revision (org-glance-config:snapshot graph))))
        (should (equal revision (org-glance-config-file:revision
                                (org-glance-config-file:read (org-glance-graph:directory graph)))))
        (org-glance-table--write-json-layout graph edits)
        (should-not (org-glance-graph:-config-error graph))
        (should (equal revision (org-glance-config-file:revision
                                (org-glance-config:snapshot graph))))
        (should (equal bytes (org-glance-table-config-test--bytes graph)))))))
