;;; test-graph.el --- Tests for `org-glance-graph'  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:graph-creation ()
  (with-temp-directory dir
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-graph? graph))
      (should (f-exists? (org-glance-graph:data-path graph)))
      (should (f-exists? (org-glance-graph:meta-path graph)))
      (should (f-exists? (org-glance-graph:headline-meta-path graph))))))

(ert-deftest org-glance-test:graph-add-get ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (headline (org-glance-headline--from-lines
                       "* TODO Test headline :foo:"
                       ":PROPERTIES:"
                       ":ORG_GLANCE_ID: test-id-123"
                       ":END:"
                       "Some content")))
      (org-glance-graph:add graph headline)
      (let ((meta (org-glance-graph:get-headline graph "test-id-123")))
        (should (org-glance-headline-metadata? meta))
        (should (string= (org-glance-headline-metadata:title meta) "Test headline"))
        (should (string= (org-glance-headline-metadata:state meta) "TODO"))))))

(ert-deftest org-glance-test:graph-headlines ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (h1 (org-glance-headline--from-lines
                 "* foo :a:"
                 ":PROPERTIES:"
                 ":ORG_GLANCE_ID: id-1"
                 ":END:"))
           (h2 (org-glance-headline--from-lines
                 "* bar :b:"
                 ":PROPERTIES:"
                 ":ORG_GLANCE_ID: id-2"
                 ":END:")))
      (org-glance-graph:add graph h1 h2)
      (let ((all (org-glance-graph:headlines graph)))
        (should (= (length all) 2))))))

(ert-deftest org-glance-test:graph-headlines-by-tag ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (h1 (org-glance-headline--from-lines
                 "* foo :alpha:"
                 ":PROPERTIES:"
                 ":ORG_GLANCE_ID: id-a"
                 ":END:"))
           (h2 (org-glance-headline--from-lines
                 "* bar :beta:"
                 ":PROPERTIES:"
                 ":ORG_GLANCE_ID: id-b"
                 ":END:")))
      (org-glance-graph:add graph h1 h2)
      (should (= (length (org-glance-graph:headlines-by-tag graph 'alpha)) 1))
      (should (= (length (org-glance-graph:headlines-by-tag graph 'beta)) 1))
      (should (= (length (org-glance-graph:headlines-by-tag graph 'gamma)) 0)))))

(ert-deftest org-glance-test:graph-delete ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (headline (org-glance-headline--from-lines
                       "* foo"
                       ":PROPERTIES:"
                       ":ORG_GLANCE_ID: del-id"
                       ":END:")))
      (org-glance-graph:add graph headline)
      (should (= (length (org-glance-graph:headlines graph)) 1))
      (org-glance-graph:delete graph "del-id")
      (should (= (length (org-glance-graph:headlines graph)) 0)))))

(ert-deftest org-glance-test:graph-store-load ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (headline (org-glance-headline--from-lines
                       "* Test store"
                       ":PROPERTIES:"
                       ":ORG_GLANCE_ID: store-id"
                       ":END:"
                       "Body text here")))
      (org-glance-graph:store-headline graph headline)
      (let ((loaded (org-glance-graph:load-headline graph "store-id")))
        (should (org-glance-headline? loaded))
        (should (string= (org-glance-headline:title loaded) "Test store"))
        (should (s-contains? "Body text here" (org-glance-headline:contents loaded)))))))

(ert-deftest org-glance-test:graph-jsonl-roundtrip ()
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (headline (org-glance-headline--from-lines
                       "* DONE roundtrip :test:"
                       ":PROPERTIES:"
                       ":ORG_GLANCE_ID: rt-id"
                       ":END:")))
      (org-glance-graph:add graph headline)

      ;; Create a new graph instance pointing to same directory to force re-read
      (remhash (f-full (file-truename dir)) org-glance-graph:list)
      (let* ((graph2 (org-glance-graph dir))
             (meta (org-glance-graph:get-headline graph2 "rt-id")))
        (should (org-glance-headline-metadata? meta))
        (should (string= (org-glance-headline-metadata:state meta) "DONE"))))))

(provide 'test-graph)
;;; test-graph.el ends here
