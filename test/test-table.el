;;; test-table.el --- Tests for the table-view consumer  -*- lexical-binding: t -*-

(require 'test-helpers)

;;; Row builder

(ert-deftest org-glance-test:table-row-from-metadata ()
  "org-glance-table--row produces a well-formed row from headline metadata."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "t1" "* TODO Alpha :work:"
                                                       "SCHEDULED: <2025-01-10 Fri> DEADLINE: <2025-02-01 Sat>"))
    (let* ((meta (car (org-glance-graph:headlines graph)))
           (row (org-glance-table--row meta))
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
           (cells (alist-get 'cells (org-glance-table--row meta))))
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
    (let* ((badges (org-glance-table--state-badges graph))
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
    (let ((spec (org-glance-table--spec graph nil)))
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
           (rows (org-glance-table--rows graph keep?))
           (ids (mapcar (lambda (r) (alist-get 'id r)) rows)))
      (should (equal '("f1") ids)))))

;;; Coherence: pull model (stale flag + display-boundary reload)

(ert-deftest org-glance-test:table-stale-flag-on-save ()
  "A materialized save FLAGS an open table stale without re-filling it on the hot
path; a display-boundary refresh re-fills it and clears the flag."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Foo :work:"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b))
                    ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph))
            (with-current-buffer buf
              (should (= 1 (length table-view--rows)))
              (should-not org-glance-view--stale)
              ;; the store advances (a new headline) and views are flagged
              (org-glance-graph:add graph (org-glance-test:headline "p2" "* TODO Bar :work:"))
              (set-file-times (org-glance-graph:headline-meta-path graph)
                              (time-add (current-time) 100))
              (org-glance-view:mark-graph-stale graph)
              ;; flagged stale, but NOT re-filled on the hot path (still 1 row)
              (should org-glance-view--stale)
              (should (= 1 (length table-view--rows)))
              ;; display boundary re-fills and clears the flag
              (org-glance-view--refresh-when-stale)
              (should (= 2 (length table-view--rows)))
              (should-not org-glance-view--stale)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

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
            (setq org-glance-table--mtime (org-glance-table--mtime src))
            (should-not (org-glance-table--stale? graph))
            (setq org-glance-table--mtime '(0 0 0 0))
            (should (org-glance-table--stale? graph)))
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

(ert-deftest org-glance-test:table-visit-default-sort ()
  "The table opens sorted by the spec default (state, active-first) regardless of
graph insertion order -- guards `org-glance-table--apply-default-sort' against
`table-view''s seed-but-don't-apply default-sort semantics."
  (org-glance-test:with-graph graph
    ;; add DONE first, so load order (d, t) differs from the sorted order (t, d)
    (org-glance-graph:add graph
                             (org-glance-test:headline "d" "* DONE Zeta")
                             (org-glance-test:headline "t" "* TODO Alpha"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b))
                    ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph))
            (with-current-buffer buf
              (should (equal '("t" "d")
                             (mapcar (lambda (r) (alist-get 'id r)) table-view--rows)))))
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
        (org-glance-table--act-materialize graph "am1" nil)
        (should (bufferp opened))
        (kill-buffer opened)))))

(ert-deftest org-glance-test:table-action-todo ()
  "The `todo' action advances the row's state (`C-c C-t' via change-todo-live);
after the (no-note) commit the reloaded table shows the new state on the row."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "td1" "* TODO Alpha"))
      (let ((buf nil))
        (unwind-protect
            (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b))
                      ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
              (setq buf (org-glance-table:visit graph))
              (with-current-buffer buf
                (org-glance-table--act-todo graph "td1" nil)   ; no-note -> synchronous finalize
                ;; persisted in the graph
                (should (equal "DONE" (org-glance-headline-metadata:state
                                       (org-glance-graph:get-headline graph "td1"))))
                ;; reloaded row reflects the new state
                (let ((row (car table-view--rows)))
                  (should (equal "td1" (alist-get 'id row)))
                  (should (equal "DONE" (alist-get 'state (alist-get 'cells row)))))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; Surgical single-row updates (buffer-text level)

(defun org-glance-test:table-1col-buffer (name rows)
  "Display a 1-column table NAME with ROWS and return the buffer."
  (let ((buf (get-buffer-create name)))
    (table-view-display buf
                        '((title . "T")
                          (columns . (((key . "n") (header . "N") (type . "text")
                                       (sortable . t) (align . "left")))))
                        nil)
    (table-view-set-rows buf rows)
    buf))

(ert-deftest org-glance-test:table-view-upsert-patch-same-width ()
  "Replacing a row with a same-width cell patches in place, keeping siblings."
  (let ((buf (org-glance-test:table-1col-buffer
              "*tv-patch-sw*"
              '(((id . "a") (cells . ((n . "one"))))
                ((id . "b") (cells . ((n . "two"))))
                ((id . "c") (cells . ((n . "six"))))))))
    (unwind-protect
        (progn
          (table-view-upsert-row buf '((id . "b") (cells . ((n . "TWO")))))
          (with-current-buffer buf
            (should (s-contains? "TWO" (buffer-string)))
            (should-not (s-contains? "two" (buffer-string)))
            (should (s-contains? "one" (buffer-string)))
            (should (s-contains? "six" (buffer-string)))
            ;; all three rows still locatable; b's row prop reflects the new cell
            (should (table-view--goto-id "a"))
            (should (table-view--goto-id "c"))
            (should (table-view--goto-id "b"))
            (should (equal "TWO" (alist-get 'n (alist-get 'cells
                                  (get-text-property (point) 'table-view-row)))))))
      (kill-buffer buf))))

(ert-deftest org-glance-test:table-view-upsert-widens-column ()
  "Replacing a row with a wider cell re-renders (full path) and keeps siblings."
  (let ((buf (org-glance-test:table-1col-buffer
              "*tv-widen*"
              '(((id . "a") (cells . ((n . "x"))))
                ((id . "b") (cells . ((n . "y"))))))))
    (unwind-protect
        (progn
          (table-view-upsert-row buf '((id . "a") (cells . ((n . "a-much-longer-value")))))
          (with-current-buffer buf
            (should (s-contains? "a-much-longer-value" (buffer-string)))
            (should (table-view--goto-id "b"))
            (should (equal "y" (alist-get 'n (alist-get 'cells
                                (get-text-property (point) 'table-view-row)))))))
      (kill-buffer buf))))

(ert-deftest org-glance-test:table-view-surgical-equals-full-render ()
  "A surgically-patched buffer is byte-identical to a full render of the same
final rows -- the equivalence the in-place fast path relies on."
  (let* ((final '(((id . "a") (cells . ((n . "one"))))
                  ((id . "b") (cells . ((n . "TWO"))))   ; b edited in place
                  ((id . "c") (cells . ((n . "six"))))))
         (surgical (org-glance-test:table-1col-buffer
                    "*tv-eq-surgical*"
                    '(((id . "a") (cells . ((n . "one"))))
                      ((id . "b") (cells . ((n . "two"))))
                      ((id . "c") (cells . ((n . "six")))))))
         (full (org-glance-test:table-1col-buffer "*tv-eq-full*" final)))
    (unwind-protect
        (progn
          ;; `surgical' reaches `final' via an in-place same-width edit; `full' is
          ;; one full render of `final'.  The visible text must be identical.
          (table-view-upsert-row surgical '((id . "b") (cells . ((n . "TWO")))))
          (should (string= (with-current-buffer surgical (buffer-string))
                           (with-current-buffer full (buffer-string)))))
      (kill-buffer surgical)
      (kill-buffer full))))

(provide 'test-table)
;;; test-table.el ends here
