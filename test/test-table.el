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

;;; Per-view persistence (column order + sort)

(cl-defmacro org-glance-test:with-table-buffer (graph var &rest body)
  "Visit GRAPH's default table, bind the buffer to VAR, run BODY, kill the buffer."
  (declare (indent 2))
  `(let ((,var nil))
     (unwind-protect
         (cl-letf (((symbol-function 'pop-to-buffer)   (lambda (b &rest _) (setq ,var b) b))
                   ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq ,var b) b)))
           (setq ,var (org-glance-table:visit ,graph))
           ,@body)
       (when (buffer-live-p ,var) (kill-buffer ,var)))))

(ert-deftest org-glance-test:table-persist-column-order ()
  "A column reorder is persisted per filter and restored on re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Alpha"))
    ;; first visit: default order has `state' first; move `title' to the front + persist
    (org-glance-test:with-table-buffer graph buf
      (with-current-buffer buf
        (should (equal "state" (alist-get 'key (car (alist-get 'columns table-view--spec)))))
        (setf (alist-get 'columns table-view--spec)
              (org-glance-table--reorder-columns (alist-get 'columns table-view--spec)
                                                 '("title" "state")))
        (org-glance-table--persist-config)))
    ;; re-visit: restored order (`title' first)
    (org-glance-test:with-table-buffer graph buf
      (with-current-buffer buf
        (should (equal "title" (alist-get 'key (car (alist-get 'columns table-view--spec)))))))))

(ert-deftest org-glance-test:table-persist-sort ()
  "A sort choice is persisted per filter and restored (applied) on re-visit."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "a" "* TODO Zeta")
                            (org-glance-test:headline "b" "* TODO Alpha"))
      ;; sort by title ascending + persist
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (setq table-view--sort-keys '(("title" . t)))
          (org-glance-table--persist-config)))
      ;; re-visit: sort restored AND applied (rows ordered by title: Alpha, Zeta)
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (should (equal '(("title" . t)) table-view--sort-keys))
          (should (equal '("b" "a") (mapcar (lambda (r) (alist-get 'id r)) table-view--rows))))))))

(ert-deftest org-glance-test:table-persist-per-filter ()
  "Configs are keyed per filter: one filter's layout does not leak to another."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "w" "* TODO A :work:")
                          (org-glance-test:headline "h" "* TODO B :home:"))
    (let ((buf nil))
      (cl-letf (((symbol-function 'pop-to-buffer)   (lambda (b &rest _) (setq buf b) b))
                ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
        (setq buf (org-glance-table:visit graph 'work))
        (with-current-buffer buf
          (setf (alist-get 'columns table-view--spec)
                (org-glance-table--reorder-columns (alist-get 'columns table-view--spec)
                                                   '("tags" "state")))
          (org-glance-table--persist-config))
        (kill-buffer buf)
        ;; the :home filter still has the default order (state first)
        (setq buf (org-glance-table:visit graph 'home))
        (with-current-buffer buf
          (should (equal "state" (alist-get 'key (car (alist-get 'columns table-view--spec))))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-visit-default-directory ()
  "The table buffer's `default-directory' is the graph ROOT (matching the
corresponding overview), not wherever the non-file buffer was spawned."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO Alpha"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq buf b) b))
                    ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph))
            (with-current-buffer buf
              (should (file-equal-p default-directory (org-glance-graph:directory graph)))))
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

(ert-deftest org-glance-test:table-m-marks-not-materializes ()
  "In the table `m' toggles the row mark (`table-view-mark-toggle'), not
materialize (which stays on RET) -- org-glance no longer binds `m'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO Alpha")
                          (org-glance-test:headline "b" "* TODO Beta"))
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer)   (lambda (b &rest _) (setq buf b) b))
                    ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
            (setq buf (org-glance-table:visit graph))
            (with-current-buffer buf
              (should (eq (key-binding (kbd "m")) #'table-view-mark-toggle))
              (should (eq (key-binding (kbd "U")) #'table-view-unmark-all))
              (should (functionp (key-binding (kbd "RET"))))   ; RET still materializes
              (table-view--goto-id "a")
              (call-interactively (key-binding (kbd "m")))
              (should (member "a" table-view--marks))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-todo-preserves-point ()
  "Changing state keeps point where it was instead of jumping to the top: when the
row leaves the view (DONE under an active filter) point stays on the same line."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "r1" "* TODO A")
                            (org-glance-test:headline "r2" "* TODO B")
                            (org-glance-test:headline "r3" "* TODO C"))
      (let ((buf nil))
        (unwind-protect
            (cl-letf (((symbol-function 'pop-to-buffer)   (lambda (b &rest _) (setq buf b) b))
                      ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq buf b) b)))
              (setq buf (org-glance-table:visit graph '(:done nil)))   ; active-only
              (with-current-buffer buf
                (should (table-view--goto-id "r2"))
                (let ((line (line-number-at-pos)))
                  (should (> line 1))                     ; r2 is below the header
                  (org-glance-table--act-todo graph "r2" nil)   ; r2 -> DONE, leaves the view
                  (should (= (line-number-at-pos) line))  ; point preserved, NOT at the top
                  (should-not (table-view--goto-id "r2"))))) ; r2 indeed gone
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest org-glance-test:table-refresh-preserves-point ()
  "`g' (refresh) keeps point on the same row even when the sort reorders it -- the
re-fill + re-sort restore by line, so without re-anchoring point drifts."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "z1" "* TODO Zeta")
                            (org-glance-test:headline "a1" "* TODO Alpha"))
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (setq table-view--sort-keys '(("title" . t)))   ; sort by title ascending
          (org-glance-table--apply-sort)                   ; order: Alpha (a1), Zeta (z1)
          (should (table-view--goto-id "z1"))              ; z1 last sorted, first in load order
          (funcall (key-binding (kbd "g")))                ; refresh
          (should (equal "z1" (get-text-property (point) 'table-view-id))))))))  ; still on z1

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

(ert-deftest org-glance-test:table-todo-candidates-fallback ()
  "Without a configured cycle, bulk candidates are the states in use."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "c1" "* TODO A")
                            (org-glance-test:headline "c2" "* DONE B"))
      (should (equal '("DONE" "TODO")           ; `org-glance-graph:states' sorts
                     (org-glance-table--todo-candidates graph nil))))))

(ert-deftest org-glance-test:table-bulk-todo-marked ()
  "`C-c C-t' with marked rows sets them all to a chosen state, then clears marks."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "b1" "* TODO A")
                            (org-glance-test:headline "b2" "* TODO B")
                            (org-glance-test:headline "b3" "* TODO C"))
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (table-view--goto-id "b1") (call-interactively #'table-view-mark-toggle)
          (table-view--goto-id "b3") (call-interactively #'table-view-mark-toggle)
          (should (= 2 (length (table-view-marked-rows))))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "DONE")))
            (funcall (key-binding (kbd "C-c C-t"))))          ; bulk (marks present)
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "b1"))))
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "b3"))))
          (should (equal "TODO" (org-glance-headline-metadata:state  ; unmarked, untouched
                                 (org-glance-graph:get-headline graph "b2"))))
          (should (null (table-view-marked-rows))))))))       ; marks cleared

(ert-deftest org-glance-test:table-bulk-todo-preserves-point ()
  "After a bulk change, point stays on the row it was on, not jumping to the top."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "p1" "* TODO A")
                            (org-glance-test:headline "p2" "* TODO B")
                            (org-glance-test:headline "p3" "* TODO C")
                            (org-glance-test:headline "p4" "* TODO D"))
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (table-view--goto-id "p1") (call-interactively #'table-view-mark-toggle)
          (table-view--goto-id "p2") (call-interactively #'table-view-mark-toggle)
          (should (table-view--goto-id "p4"))            ; park point on an UNMARKED row
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "DONE")))
            (funcall (key-binding (kbd "C-c C-t"))))     ; bulk-sets p1,p2 (not p4)
          ;; point followed its row (p4), which survived the change
          (should (equal "p4" (get-text-property (point) 'table-view-id)))
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "p1"))))
          (should (equal "TODO" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "p4")))))))))

(ert-deftest org-glance-test:table-bulk-todo-logs-and-keeps-point ()
  "The reported scenario end-to-end: bulk `C-c C-t' under timestamp logging sets
every marked row WITH its LOGBOOK entry, clears the marks, keeps point on the row
it was on -- and never errors on a dangling log marker.  (`org-glance-table:visit'
is used directly, without stubbing `pop-to-buffer', so `org-add-log-note's own
buffer switching -- which the flush relies on -- runs for real.)"
  (let ((org-todo-keywords '((sequence "TODO" "DONE(!)")))   ; `!' logs a timestamp
        (org-log-into-drawer nil) (this-command 'org-glance-test-bulk) (buf nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "g1" "* TODO A")
                            (org-glance-test:headline "g2" "* TODO B")
                            (org-glance-test:headline "g3" "* TODO C"))
      (unwind-protect
          (progn
            (setq buf (org-glance-table:visit graph))
            (with-current-buffer buf
              (table-view--goto-id "g1") (call-interactively #'table-view-mark-toggle)
              (table-view--goto-id "g2") (call-interactively #'table-view-mark-toggle)
              (table-view--goto-id "g3")                     ; point on the UNMARKED row
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "DONE")))
                (funcall (key-binding (kbd "C-c C-t"))))
              (run-hooks 'post-command-hook)                 ; must NOT dead-marker
              (should (equal "g3" (get-text-property (point) 'table-view-id)))  ; point kept
              (should (null (table-view-marked-rows)))       ; marks cleared
              (dolist (id '("g1" "g2"))                      ; both marked: DONE + logged
                (should (equal "DONE" (org-glance-headline-metadata:state
                                       (org-glance-graph:get-headline graph id))))
                (should (s-contains? "State \"DONE\""
                                     (org-glance-graph:get-content graph id))))
              (should (equal "TODO" (org-glance-headline-metadata:state  ; unmarked untouched
                                     (org-glance-graph:get-headline graph "g3"))))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-todo-single-when-unmarked ()
  "With no marks, `C-c C-t' takes the single-row path (advances the point row)."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "s1" "* TODO A"))
      (org-glance-test:with-table-buffer graph buf
        (with-current-buffer buf
          (should (table-view--goto-id "s1"))
          (should (null (table-view-marked-rows)))
          (funcall (key-binding (kbd "C-c C-t")))             ; single (no marks)
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "s1")))))))))

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

;;; Custom property columns (`C-u +' adds, `-' removes, persisted per tag)

(cl-defmacro org-glance-test:with-table-filter (graph filter var &rest body)
  "Visit GRAPH's table for FILTER, bind the buffer to VAR, run BODY, kill it."
  (declare (indent 3))
  `(let ((,var nil))
     (unwind-protect
         (cl-letf (((symbol-function 'pop-to-buffer)   (lambda (b &rest _) (setq ,var b) b))
                   ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq ,var b) b)))
           (setq ,var (org-glance-table:visit ,graph ,filter))
           ,@body)
       (when (buffer-live-p ,var) (kill-buffer ,var)))))

(defun org-glance-test:table-col-keys (buf)
  "Display-order column keys of table BUF."
  (with-current-buffer buf
    (mapcar (lambda (c) (alist-get 'key c)) (table-view--columns table-view--spec))))

(defun org-glance-test:table-cell (buf id key)
  "Cell KEY of row ID in table BUF."
  (with-current-buffer buf
    (table-view--cell (cl-find id table-view--rows
                               :key (lambda (r) (alist-get 'id r)) :test #'equal)
                      key)))

(ert-deftest org-glance-test:table-schema-key-by-tags ()
  "The schema key is the filter's tags, sorted and joined; none -> \":none:\"."
  (should (equal "book" (org-glance-table--schema-key '(:tags ("book")))))
  (should (equal "article+book" (org-glance-table--schema-key '(:tags ("book" "article")))))
  (should (equal ":none:" (org-glance-table--schema-key nil))))

(ert-deftest org-glance-test:table-add-property-column-shows-drawer-value ()
  "An added property column displays each headline's drawer property."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO The Hobbit :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: bk1" ":AUTHOR: Tolkien" ":END:")
      (org-glance-headline--from-lines
       "* TODO Dune :book:"       ":PROPERTIES:" ":ORG_GLANCE_ID: bk2" ":AUTHOR: Herbert" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR")))
      (should (member "AUTHOR" (org-glance-test:table-col-keys buf)))
      (should (equal "Tolkien" (org-glance-test:table-cell buf "bk1" "AUTHOR")))
      (should (equal "Herbert" (org-glance-test:table-cell buf "bk2" "AUTHOR")))
      (with-current-buffer buf
        (should (string-match-p "Author"  (buffer-string)))
        (should (string-match-p "Tolkien" (buffer-string)))))))

(ert-deftest org-glance-test:table-property-column-persists-per-tag ()
  "An added property column is saved per tag and restored (with values) on re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO The Hobbit :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: bk1" ":AUTHOR: Tolkien" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))))
    ;; re-visit :book -> the AUTHOR column comes back, still reading the drawer
    (org-glance-test:with-table-filter graph 'book buf
      (should (member "AUTHOR" (org-glance-test:table-col-keys buf)))
      (should (equal "Tolkien" (org-glance-test:table-cell buf "bk1" "AUTHOR"))))))

(ert-deftest org-glance-test:table-property-column-per-tag-isolation ()
  "A column added under one tag does not appear under another."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO The Hobbit :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: bk1" ":AUTHOR: Tolkien" ":END:")
      (org-glance-headline--from-lines
       "* TODO Ship it :work:"    ":PROPERTIES:" ":ORG_GLANCE_ID: wk1" ":AUTHOR: Me" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))))
    (org-glance-test:with-table-filter graph 'work buf
      (should-not (member "AUTHOR" (org-glance-test:table-col-keys buf))))))

(ert-deftest org-glance-test:table-remove-property-column ()
  "Removing an added column drops it, and the removal persists across a re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO The Hobbit :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: bk1" ":AUTHOR: Tolkien" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))
        (should (equal '(("AUTHOR" . "Author")) (org-glance-table--schema-get graph 'book)))
        (table-view-remove-column "AUTHOR")
        (should-not (org-glance-table--schema-get graph 'book))))
    (org-glance-test:with-table-filter graph 'book buf
      (should-not (member "AUTHOR" (org-glance-test:table-col-keys buf))))))

(ert-deftest org-glance-test:table-add-column-function-wired ()
  "The visited buffer wires `C-u +' to the drawer-property prompt."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO X :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: x1" ":AUTHOR: A" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (should (eq table-view-add-column-function #'org-glance-table--add-column-prompt))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "author")))
          (let ((col (org-glance-table--add-column-prompt)))
            (should (equal "AUTHOR" (alist-get 'key col)))
            (should (equal "Author" (alist-get 'header col)))
            (should (functionp (alist-get 'value-fn col)))))))))

(ert-deftest org-glance-test:table-cu-plus-adds-column ()
  "Pressing `C-u +' (prefix arg + the capture key) adds a property column."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines
       "* TODO X :book:" ":PROPERTIES:" ":ORG_GLANCE_ID: x1" ":AUTHOR: Ann" ":END:"))
    (org-glance-test:with-table-filter graph 'book buf
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "AUTHOR")))
          (let ((current-prefix-arg '(4)))
            (funcall (key-binding (kbd "+")))))     ; the `+' action reads current-prefix-arg
        (should (member "AUTHOR" (org-glance-test:table-col-keys buf)))
        (should (equal "Ann" (org-glance-test:table-cell buf "x1" "AUTHOR")))))))

(provide 'test-table)
;;; test-table.el ends here
