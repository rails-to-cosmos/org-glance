;;; test-tags.el --- Tests for the all-tags overview  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:tags-metrics-live-under-meta ()
  "Each process writes its own tag-metrics segment under meta/."
  (org-glance-test:with-graph graph
    (let ((org-glance-tag-metrics--session-id
           "00000000-0000-0000-0000-000000000001"))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:"))
      (let ((path (org-glance-tag-metrics--file graph)))
        (should (string-prefix-p (file-name-as-directory
                                  (org-glance-graph:meta-path graph))
                                 path))
        (should (f-exists? path))
        (should-not (f-exists? (org-glance-graph:config-file
                                graph "tag-metrics.eld")))))))

(ert-deftest org-glance-test:tags-metrics-worktrees-have-distinct-writers ()
  "One Emacs process gives separate store paths separate writer segments."
  (with-temp-directory first
    (with-temp-directory second
      (let ((a (org-glance-graph first))
            (b (org-glance-graph second)))
        (should-not
         (equal (file-name-nondirectory (org-glance-tag-metrics--file a))
                (file-name-nondirectory (org-glance-tag-metrics--file b))))))))

(ert-deftest org-glance-test:tags-metrics-tracked ()
  "Adding headlines records per-tag count, states, timestamps and captures."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:y:")
                          (org-glance-test:headline "b" "* DONE B :x:"))
    (let* ((all (org-glance-tag-metrics:all graph))
           (x (cdr (assoc "x" all)))
           (y (cdr (assoc "y" all))))
      (should (= 2 (plist-get x :count)))
      (should (= 1 (plist-get y :count)))
      (should (plist-get x :created))
      (should (plist-get x :modified))
      (should (= 2 (plist-get x :captures)))
      (should (equal 1 (alist-get "TODO" (plist-get x :states) nil nil #'string=)))
      (should (equal 1 (alist-get "DONE" (plist-get x :states) nil nil #'string=))))))

(ert-deftest org-glance-test:tags-metrics-created-once ()
  "`:created' is stamped at first sighting; later adds bump count and captures."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:"))
    (let ((created1 (plist-get (cdr (assoc "x" (org-glance-tag-metrics:all graph))) :created)))
      (should created1)
      (org-glance-graph:add graph (org-glance-test:headline "b" "* B :x:"))
      (let ((m (cdr (assoc "x" (org-glance-tag-metrics:all graph)))))
        (should (equal created1 (plist-get m :created)))   ; unchanged
        (should (= 2 (plist-get m :count)))
        (should (= 2 (plist-get m :captures)))))))

(ert-deftest org-glance-test:tags-metrics-removal-counter ()
  "Deleting a headline bumps :removals for its tags."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:"))
    (org-glance-graph:delete graph "a")
    (let ((m (cdr (assoc "x" (org-glance-tag-metrics--read graph)))))
      (should (= 1 (plist-get m :removals))))
    (should-not (member "x" (org-glance-graph:tags graph)))))

(ert-deftest org-glance-test:tags-metrics-writer-segments-sum ()
  "Disjoint writer segments preserve concurrent increments exactly."
  (org-glance-test:with-graph graph
    (let ((org-glance-tag-metrics--session-id
           "00000000-0000-0000-0000-000000000001"))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:")))
    (let ((org-glance-tag-metrics--session-id
           "00000000-0000-0000-0000-000000000002"))
      (org-glance-graph:add graph (org-glance-test:headline "b" "* B :x:")))
    (let ((m (cdr (assoc "x" (org-glance-tag-metrics--read graph)))))
      (should (= 2 (plist-get m :captures))))))

(ert-deftest org-glance-test:tags-metrics-migrates-config-singleton ()
  "The old config singleton becomes a content-addressed metadata baseline."
  (org-glance-test:with-graph graph
    (let ((legacy (org-glance-graph:config-file graph "tag-metrics.eld")))
      (org-glance--write-eld legacy '(("x" :captures 7 :removals 2)))
      (let ((m (cdr (assoc "x" (org-glance-tag-metrics--read graph)))))
        (should (= 7 (plist-get m :captures)))
        (should (= 2 (plist-get m :removals))))
      (should-not (f-exists? legacy))
      (should (= 1 (length (org-glance-tag-metrics--files
                            graph org-glance-tag-metrics--legacy-name-re)))))))

(ert-deftest org-glance-test:tags-metrics-legacy-baselines-do-not-double-count ()
  "Divergent singleton migrations merge before current components are added."
  (org-glance-test:with-graph graph
    (let ((meta (org-glance-graph:meta-path graph)))
      (org-glance--write-eld
       (f-join meta (concat "tag-metrics-legacy-" (make-string 40 ?a) ".eld"))
       '(("x" :captures 5)))
      (org-glance--write-eld
       (f-join meta (concat "tag-metrics-legacy-" (make-string 40 ?b) ".eld"))
       '(("x" :captures 7)))
      (org-glance--write-eld (org-glance-tag-metrics--file graph)
                             '(("x" :captures 2)))
      (should (= 9 (plist-get (cdr (assoc "x" (org-glance-tag-metrics--read graph)))
                              :captures))))))

(ert-deftest org-glance-test:tags-rows ()
  "`org-glance-tags--rows' yields one row per tag, id = tag, count in cells."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:")
                          (org-glance-test:headline "b" "* TODO B :y:"))
    (let* ((rows (org-glance-tags--rows graph))
           (ids (org-glance-test:row-ids rows)))
      (should (= 2 (length rows)))
      (should (member "x" ids))
      (should (member "y" ids))
      (let ((xr (cl-find "x" rows :key (lambda (r) (alist-get 'id r)) :test #'string=)))
        (should (equal "1" (alist-get 'count (alist-get 'cells xr))))))))

(ert-deftest org-glance-test:tags-retag-remove ()
  "Removing a tag strips it everywhere; single-tagged headlines stay live."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:y:")   ; multi
                          (org-glance-test:headline "b" "* TODO B :x:"))    ; single
    ;; the row id is a STRING -- what the `-' action forwards.
    (let ((res (org-glance-tags--retag-remove graph "x" '("a" "b"))))
      (should (equal '(2 . 0) res))
      (should-not (member "x" (org-glance-graph:tags graph)))
      (should (member "y" (org-glance-graph:tags graph)))
      (should (equal '("y") (org-glance-test:field graph "a" tags)))
      (should (null (org-glance-test:field graph "b" tags))))))

(ert-deftest org-glance-test:tags-visit ()
  "`org-glance-tags:visit' opens the single tags buffer with one row per tag."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:")
                          (org-glance-test:headline "b" "* TODO B :y:"))
    (let ((org-glance-graph graph)
          (org-glance-view-fill-frame nil))
      (org-glance-test:with-open buf (org-glance-tags:visit graph)
        (with-current-buffer buf
          (should (string= "*org-glance-tags*" (buffer-name)))
          (should (= 2 (length table-view--rows))))))))

(ert-deftest org-glance-test:tags-states-colored ()
  "The States cell colours each state name with a todo-state face."
  (let ((s (org-glance-tags--format-states '(("TODO" . 2) ("DONE" . 1)))))
    (should (s-contains? "TODO" s))
    (should (get-text-property (string-match "TODO" s) 'face s))
    (should (get-text-property (string-match "DONE" s) 'face s))))

(ert-deftest org-glance-test:tags-cycle-colored ()
  "The Cycle cell colours each keyword; the `|' separator stays plain."
  (let ((s (org-glance-tags--format-cycle "TODO | DONE")))
    (should (get-text-property (string-match "TODO" s) 'face s))
    (should (get-text-property (string-match "DONE" s) 'face s))
    (should-not (get-text-property (string-match "|" s) 'face s))))

(ert-deftest org-glance-test:tags-filter-overlays-ambient ()
  "A dashboard tag filter overlays the ambient spec, as o/table pickers do."
  (should (equal '(:done nil :archived nil :commented nil :tags ("x"))
                 (org-glance-tags--tag-filter "x"))))

(ert-deftest org-glance-test:tags-ret-opens-table ()
  "RET on a tag row opens that tag's `org-glance-table' buffer."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A :x:"))
    (let ((org-glance-graph graph)
          (org-glance-view-fill-frame nil))
      (org-glance-test:with-open buf (org-glance-tags--act-table graph "x")
        (should (string-prefix-p "*org-glance-table:" (buffer-name buf)))))))

(provide 'test-tags)
;;; test-tags.el ends here
