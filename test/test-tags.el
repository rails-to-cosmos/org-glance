;;; test-tags.el --- Tests for the all-tags overview  -*- lexical-binding: t -*-

(require 'test-helpers)

;;; Per-tag metrics (event-tracked sidecar)

(ert-deftest org-glance-test:tags-metrics-tracked ()
  "Adding headlines records per-tag count, states, timestamps and capture counter."
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
      ;; state breakdown for x: one TODO, one DONE
      (should (equal 1 (alist-get "TODO" (plist-get x :states) nil nil #'string=)))
      (should (equal 1 (alist-get "DONE" (plist-get x :states) nil nil #'string=))))))

(ert-deftest org-glance-test:tags-metrics-created-once ()
  "`:created' is stamped once (first sighting); later events bump count/captures."
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
    ;; x has no live headline now, so it drops out of the derived tag set
    (should-not (member "x" (org-glance-graph:tags graph)))))

;;; Row builder

(ert-deftest org-glance-test:tags-rows ()
  "`org-glance-tags--rows' yields one row per tag, id = tag, count in cells."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:")
                          (org-glance-test:headline "b" "* TODO B :y:"))
    (let* ((rows (org-glance-tags--rows graph))
           (ids (mapcar (lambda (r) (alist-get 'id r)) rows)))
      (should (= 2 (length rows)))
      (should (member "x" ids))
      (should (member "y" ids))
      (let ((xr (cl-find "x" rows :key (lambda (r) (alist-get 'id r)) :test #'string=)))
        (should (equal "1" (alist-get 'count (alist-get 'cells xr))))))))

;;; Remove tag (non-destructive retag)

(ert-deftest org-glance-test:tags-retag-remove ()
  "Removing a tag drops it off each headline: multi-tagged survive, single-tagged
become untagged but live, and the tag leaves the derived tag set."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :x:y:")   ; multi
                          (org-glance-test:headline "b" "* TODO B :x:"))    ; single
    (let ((res (org-glance-tags--retag-remove graph 'x '("a" "b"))))
      (should (equal '(2 . 0) res))
      (should-not (member "x" (org-glance-graph:tags graph)))
      (should (member "y" (org-glance-graph:tags graph)))
      (should (equal '("y") (org-glance-headline-metadata:tags
                             (org-glance-graph:get-headline graph "a"))))
      (should (null (org-glance-headline-metadata:tags
                     (org-glance-graph:get-headline graph "b")))))))

;;; Visit

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

;;; Coloured cells

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

;;; RET -> tag's headline table

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
