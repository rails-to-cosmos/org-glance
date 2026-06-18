;;; test-table.el --- Tests for the table-view consumer  -*- lexical-binding: t -*-

(require 'test-helpers)

;;; table-view core: remove-row primitive

(ert-deftest org-glance-test:table-view-remove-row ()
  "table-view-remove-row drops the matching row by id and re-renders."
  (let ((buf (get-buffer-create "*tv-remove-test*")))
    (unwind-protect
        (progn
          (table-view-display buf
                             '((title . "T")
                               (columns . (((key . "n") (header . "N") (type . "text") (sortable . t) (align . "left")))))
                             nil)
          (table-view-set-rows buf '(((id . "a") (cells . ((n . "one"))))
                                     ((id . "b") (cells . ((n . "two"))))))
          (with-current-buffer buf
            (should (= 2 (length table-view--rows))))
          (table-view-remove-row buf "a")
          (with-current-buffer buf
            (should (= 1 (length table-view--rows)))
            (should (equal "b" (alist-get 'id (car table-view--rows))))))
      (kill-buffer buf))))

(ert-deftest org-glance-test:table-view-remove-row-noop ()
  "table-view-remove-row is a no-op when the id is absent."
  (let ((buf (get-buffer-create "*tv-remove-noop*")))
    (unwind-protect
        (progn
          (table-view-display buf
                             '((title . "T")
                               (columns . (((key . "n") (header . "N") (type . "text") (sortable . t) (align . "left")))))
                             nil)
          (table-view-set-rows buf '(((id . "a") (cells . ((n . "one"))))))
          (table-view-remove-row buf "nonexistent")
          (with-current-buffer buf
            (should (= 1 (length table-view--rows)))))
      (kill-buffer buf))))

;;; Row builder

(ert-deftest org-glance-test:table-row-from-metadata ()
  "org-glance-table:--row produces a well-formed row from headline metadata."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "t1" "* TODO Alpha :work:"
                                                       "SCHEDULED: <2025-01-10 Fri> DEADLINE: <2025-02-01 Sat>"))
    (let* ((meta (car (org-glance-graph:headlines graph)))
           (row (org-glance-table:--row meta))
           (cells (alist-get 'cells row)))
      (should (equal "t1" (alist-get 'id row)))
      (should (equal "TODO" (alist-get 'state cells)))
      (should (equal "Alpha" (alist-get 'title cells)))
      (should (s-contains? "work" (alist-get 'tags cells)))
      (should (s-contains? "2025-01-10" (alist-get 'schedule cells)))
      (should (s-contains? "2025-02-01" (alist-get 'deadline cells))))))

(ert-deftest org-glance-test:table-row-empty-fields ()
  "Missing state/tags/planning render as empty strings, not nil."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "t2" "* Plain"))
    (let* ((meta (car (org-glance-graph:headlines graph)))
           (cells (alist-get 'cells (org-glance-table:--row meta))))
      (should (equal "" (alist-get 'state cells)))
      (should (equal "" (alist-get 'tags cells)))
      (should (equal "" (alist-get 'schedule cells)))
      (should (equal "" (alist-get 'deadline cells)))
      (should (equal "" (alist-get 'priority cells))))))

;;; Badge palette

(ert-deftest org-glance-test:table-state-badges-active-before-done ()
  "State badges list active states before done states."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "b1" "* TODO Active")
                             (org-glance-test:headline "b2" "* DONE Finished"))
    (let* ((badges (org-glance-table:--state-badges graph))
           (values (mapcar (lambda (b) (alist-get 'value b)) badges)))
      (should (member "TODO" values))
      (should (member "DONE" values))
      (should (< (cl-position "TODO" values :test #'equal)
                 (cl-position "DONE" values :test #'equal))))))

;;; Spec generation

(ert-deftest org-glance-test:table-spec-shape ()
  "The spec has the required top-level keys and six columns."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "s1" "* TODO X"))
    (let ((spec (org-glance-table:--spec graph nil)))
      (should (alist-get 'title spec))
      (should (= 6 (length (alist-get 'columns spec))))
      (should (alist-get 'actions spec))
      (should (alist-get 'sort spec)))))

;;; Filter

(ert-deftest org-glance-test:table-fill-honours-filter ()
  "Only headlines matching the filter appear as rows."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "f1" "* Alpha :work:")
                             (org-glance-test:headline "f2" "* Beta :home:"))
    (let* ((keep? (org-glance-filter:predicate '(:tags ("work"))))
           (rows (org-glance-table:--rows graph keep?))
           (ids (mapcar (lambda (r) (alist-get 'id r)) rows)))
      (should (equal '("f1") ids)))))

;;; Coherence: patch (eager)

(ert-deftest org-glance-test:table-patch-upserts-matching ()
  "A metadata that still matches the filter is upserted into the table."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Foo :work:"))
    (let ((buf (get-buffer-create "*tv-patch-up*")))
      (unwind-protect
          (progn
            (table-view-display buf
                               (org-glance-table:--spec graph nil)
                               nil)
            (with-current-buffer buf
              (setq org-glance-table--graph graph
                    org-glance-table--spec nil)
              (let ((meta (car (org-glance-graph:headlines graph))))
                (org-glance-table:--patch meta)
                (should (= 1 (length table-view--rows)))
                (should (equal "p1" (alist-get 'id (car table-view--rows)))))))
        (kill-buffer buf)))))

(ert-deftest org-glance-test:table-patch-removes-non-matching ()
  "A metadata that no longer matches the filter is removed from the table."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "r1" "* TODO Foo :work:")
                             (org-glance-test:headline "r2" "* TODO Bar :home:"))
    (let* ((keep? (org-glance-filter:predicate '(:tags ("work"))))
           (rows (org-glance-table:--rows graph keep?))
           (buf (get-buffer-create "*tv-patch-rm*")))
      (unwind-protect
          (progn
            (table-view-display buf
                               (org-glance-table:--spec graph '(:tags ("work")))
                               nil)
            (table-view-set-rows buf rows)
            (with-current-buffer buf
              (setq org-glance-table--graph graph
                    org-glance-table--spec '(:tags ("work")))
              (should (= 1 (length table-view--rows)))
              (let ((home-meta (cadr (org-glance-graph:headlines graph))))
                (org-glance-table:--patch home-meta)
                (should (= 1 (length table-view--rows)))
                (should (equal "r1" (alist-get 'id (car table-view--rows)))))))
        (kill-buffer buf)))))

;;; Coherence: staleness

(ert-deftest org-glance-test:table-stale-detection ()
  "A table is stale when the store's mtime advances past its recorded mtime."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO X"))
    (let ((src (org-glance-graph:headline-meta-path graph))
          (buf (get-buffer-create "*tv-stale*")))
      (unwind-protect
          (with-current-buffer buf
            (table-view-mode)
            (setq org-glance-table--graph graph)
            (setq org-glance-table--mtime (org-glance-table:--mtime src))
            (should-not (org-glance-table:--stale?))
            (setq org-glance-table--mtime '(0 0 0 0))
            (should (org-glance-table:--stale?)))
        (kill-buffer buf)))))

;;; Visit + actions

(ert-deftest org-glance-test:table-visit-creates-buffer ()
  "org-glance-table:visit creates a table-view buffer with rows from the graph."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "v1" "* TODO One")
                             (org-glance-test:headline "v2" "* DONE Two"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (derived-mode-p 'table-view-mode))
              (should (= 2 (length table-view--rows)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-visit-tag-filter ()
  "org-glance-table:visit with a tag filter shows only matching headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "vt1" "* TODO Alpha :work:")
                             (org-glance-test:headline "vt2" "* TODO Beta :home:"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph 'work))
            (with-current-buffer buf
              (should (= 1 (length table-view--rows)))
              (should (equal "vt1" (alist-get 'id (car table-view--rows))))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-action-materialize ()
  "The materialize action handler opens via org-glance-material:open."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "am1" "* TODO Alpha"))
    (let ((opened nil))
      (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq opened b) b))
                ((symbol-function 'org-glance-material:open) (lambda (_g id) (get-buffer-create (format "*mat-%s*" id)))))
        (org-glance-table:--act-materialize graph "am1" nil)
        (should (bufferp opened))
        (kill-buffer opened)))))

(provide 'test-table)
;;; test-table.el ends here
