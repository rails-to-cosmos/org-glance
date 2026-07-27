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

(defface org-glance-test--review-face '((t :foreground "gold"))
  "Fixture face for the org-todo-keyword-faces fallback test.")

(ert-deftest org-glance-test:table-state-color-from-org-faces ()
  "A state absent from the table palette takes its colour from the user's
`org-todo-keyword-faces' -- colour string, plist, or face symbol -- before
falling back to the default."
  (let ((org-todo-keyword-faces '(("DELEGATED" . "orchid")
                                  ("BLOCKED" . (:foreground "tomato" :weight bold))
                                  ("REVIEW" . org-glance-test--review-face))))
    (should (equal "orchid" (org-glance-table--state-color "DELEGATED")))
    (should (equal "tomato" (org-glance-table--state-color "BLOCKED")))
    (should (equal "gold" (org-glance-table--state-color "REVIEW")))
    ;; the explicit palette still wins, and unknowns still get the default
    (should (equal (cdr (assoc "TODO" org-glance-table-state-colors))
                   (org-glance-table--state-color "TODO")))
    (should (equal org-glance-table-default-state-color
                   (org-glance-table--state-color "NOWHERE")))))

;;; Spec generation

(ert-deftest org-glance-test:table-spec-shape ()
  "The spec has the required top-level keys and exactly the built-in columns."
  (org-glance-test:with-graph graph
    (let ((spec (org-glance-table--spec graph nil)))
      ;; `title'/`actions'/`sort' are literals of the same backquote -- only
      ;; the column set is a real claim (the built-ins, in default order).
      (should (equal '("state" "title" "schedule" "deadline" "interval"
                       "priority" "encrypted" "repeated" "tags")
                     (mapcar (lambda (c) (alist-get 'key c))
                             (alist-get 'columns spec)))))))

;;; #+TODO: header line (always-visible subtitle)

(ert-deftest org-glance-test:table-todo-line ()
  "The spec carries a `#+TODO:'-style subtitle of the graph's states -- active,
then `|', then done -- coloured; exposed as the `table-view' subtitle."
  (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
        (org-done-keywords '("DONE")))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "a" "* TODO A")
                            (org-glance-test:headline "b" "* NEXT B")
                            (org-glance-test:headline "c" "* DONE C"))
      (let* ((line (org-glance-table--todo-line graph))
             (plain (substring-no-properties line)))
        (should (string-prefix-p "#+TODO: " plain))
        (should (s-contains? "TODO" plain))
        (should (s-contains? "NEXT" plain))
        (should (s-contains? "| DONE" plain))              ; done after the bar
        (should (text-property-not-all 0 (length line) 'face nil line))  ; coloured
        ;; the spec exposes it as the always-visible subtitle
        (should (equal line (alist-get 'subtitle (org-glance-table--spec graph nil))))))))

;;; Filter

(ert-deftest org-glance-test:table-fill-honours-filter ()
  "Only headlines matching the filter appear as rows."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "f1" "* Alpha :work:")
                             (org-glance-test:headline "f2" "* Beta :home:"))
    (let* ((keep? (org-glance-filter:predicate '(:tags ("work"))))
           (rows (org-glance-table--rows graph keep?))
           (ids (org-glance-test:row-ids rows)))
      (should (equal '("f1") ids)))))

;;; Coherence: pull model (stale flag + display-boundary reload)

(ert-deftest org-glance-test:table-stale-flag-on-save ()
  "A materialized save FLAGS an open table stale without re-filling it on the hot
path; a display-boundary refresh re-fills it and clears the flag."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Foo :work:"))
    (org-glance-test:with-table (graph)
        (should (= 1 (length table-view--rows)))
        (should-not org-glance-view--stale)
        ;; the store advances (a new headline) and views are flagged
        (org-glance-graph:add graph (org-glance-test:headline "p2" "* TODO Bar :work:"))
        (org-glance-test:store-mtime graph 100)
        (org-glance-view:mark-graph-stale graph)
        ;; flagged stale, but NOT re-filled on the hot path (still 1 row)
        (should org-glance-view--stale)
        (should (= 1 (length table-view--rows)))
        ;; display boundary re-fills and clears the flag
        (org-glance-view--refresh-when-stale)
        (should (= 2 (length table-view--rows)))
        (should-not org-glance-view--stale))))

;;; Coherence: staleness

(ert-deftest org-glance-test:table-stale-detection ()
  "A table is stale when the store's mtime advances past its recorded mtime."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO X"))
    (let ((src (org-glance-graph:headline-meta-path graph)))
      (org-glance-test:with-open buf (get-buffer-create "*tv-stale*")
        (with-current-buffer buf
          (table-view-mode)
          (org-glance-view:snapshot-mtime src)
          (should-not (org-glance-view:stale-vs-file? src))
          (setq org-glance-view--mtime '(0 0 0 0))
          (should (org-glance-view:stale-vs-file? src)))))))

;;; Visit + actions

(ert-deftest org-glance-test:table-visit-creates-buffer ()
  "org-glance-table:visit creates a table-view buffer with rows from the graph."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "v1" "* TODO One")
                             (org-glance-test:headline "v2" "* DONE Two"))
    (org-glance-test:with-table (graph)
      (should (derived-mode-p 'table-view-mode))
      (should (= 2 (length table-view--rows)))
      ;; `!' quietly aliases the `j' open action (dired rhyme)
      (should (eq (key-binding (kbd "!")) (key-binding (kbd "j"))))
      (should (key-binding (kbd "j"))))))

(ert-deftest org-glance-test:table-relations-key ()
  "`@' in the table and the overview opens the row's relation table, anchored
on it; a headline related to nothing errors instead."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "[[org-glance-material:b][B]]")
      (org-glance-test:headline "b" "* TODO B")
      (org-glance-test:headline "z" "* TODO Z"))       ; related to nothing
    (let (calls)
      (org-glance-test:with-table (graph)
        (should (key-binding (kbd "@")))
        (table-view--goto-id "a")
        ;; stub only the pivot itself -- the table buffer above needs the real one
        (cl-letf (((symbol-function 'org-glance-table:visit)
                   (lambda (_g filter &rest args)
                     (push (cons filter (plist-get args :context)) calls)
                     nil)))
          (funcall (key-binding (kbd "@")))))
      (should (equal '(:id-any ("b")) (car (car calls))))
      (should (equal '(:anchor "a" :dir relations) (cdr (car calls)))))
    ;; a row with no relations errors through the same key
    (org-glance-test:with-table (graph)
      (table-view--goto-id "z")
      (should-error (funcall (key-binding (kbd "@"))) :type 'user-error))
    ;; the overview's `@' routes to the same entry point
    (should (eq (lookup-key org-glance-overview-mode-map (kbd "@"))
                #'org-glance-overview:relations))))

(ert-deftest org-glance-test:table-remove-column-by-name ()
  "`C-c -' removes a column chosen by name; the mandatory Title is never
offered, and the removal persists per tag."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A :work:"))
    (org-glance-test:with-table (graph '(:tags ("work")))
      (should (eq (key-binding (kbd "C-c -")) #'org-glance-table:remove-column))
      (should (member "tags" (org-glance-test:table-col-keys)))
      ;; point sits ON a column, yet `C-u' still prompts
      (org-glance-test:goto-cell "a" "state")
      (org-glance-test:offering (offered "Tags")
        (org-glance-table:remove-column '(4))
        ;; Title is mandatory -- it is not among the candidates
        (should-not (member "Title" offered))
        (should (member "Tags" offered)))
      (should-not (member "tags" (org-glance-test:table-col-keys))))
    ;; persisted per tag: a fresh view of the same tag stays without it
    (org-glance-test:with-table (graph '(:tags ("work")))
      (should-not (member "tags" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:table-refresh-resets-filter ()
  "`g\' returns the table to the filter it was opened with: the `/\' substring
filter and a narrow-to-marked view are both dropped, while the view\'s own
filter still governs which rows exist.  An action-triggered reload keeps them."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a1" "* TODO Alpha :work:")
      (org-glance-test:headline "b1" "* TODO Beta :work:")
      (org-glance-test:headline "c1" "* TODO Gamma :home:"))
    (org-glance-test:with-table (graph '(:tags ("work")))
      (should (= 2 (length (table-view--visible-rows))))    ; the view's own filter
      (table-view-filter "alpha")
      (should (equal "alpha" table-view--filter))
      (should (= 1 (length (table-view--visible-rows))))
      ;; a reload behind an edit PRESERVES the refinement
      (org-glance-table--reload (current-buffer))
      (should (equal "alpha" table-view--filter))
      ;; `g' resets it -- back to the initial filter, not to every headline
      (funcall (key-binding (kbd "g")))
      (should-not table-view--filter)
      (should (= 2 (length (table-view--visible-rows))))
      (should (equal '("a1" "b1")
                     (sort (mapcar (lambda (r) (alist-get 'id r)) table-view--rows)
                           #'string<)))
      ;; a narrow-to-marked view is a refinement too
      (table-view--goto-id "a1")
      (table-view-mark-toggle)
      (table-view-narrow-toggle)
      (should table-view--narrowed)
      (funcall (key-binding (kbd "g")))
      (should-not table-view--narrowed)
      (should (= 2 (length (table-view--visible-rows)))))))

(ert-deftest org-glance-test:table-filter-reset ()
  "`C-u /' clears the active substring filter; `/' is bound to the wrapper."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "r1" "* TODO Alpha")
                          (org-glance-test:headline "r2" "* TODO Beta"))
    (org-glance-test:with-table (graph)
        (should (eq (key-binding "/") #'org-glance-table:filter-or-reset))
        (table-view-filter "alpha")
        (should (equal "alpha" table-view--filter))
        (let ((current-prefix-arg '(4)))       ; `C-u /'
          (org-glance-table:filter-or-reset))
        (should (null table-view--filter)))))

(ert-deftest org-glance-test:table-add-tag ()
  "`:' adds a tag (new tags allowed) to the headline at point and persists it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "t1" "* TODO Alpha :work:"))
    (org-glance-test:with-table (graph)
        (let ((id (org-glance-test:first-row-id)))
          (org-glance-test:answering ((completing-read "urgent"))
            (org-glance-table--act-tag graph id))
          (should (member "urgent"
                          (org-glance-headline-metadata:tag-strings
                           (org-glance-graph:get-headline graph id))))))))

(ert-deftest org-glance-test:table-remove-tag ()
  "`C-u :' removes one of the headline's own tags, keeping the others."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "t2" "* TODO Beta :work:home:"))
    (org-glance-test:with-table (graph)
        (let ((id (org-glance-test:first-row-id)))
          (org-glance-test:answering ((completing-read "work"))
            (let ((current-prefix-arg '(4)))
              (org-glance-table--act-tag graph id)))
          (let ((tags (org-glance-headline-metadata:tag-strings
                       (org-glance-graph:get-headline graph id))))
            (should-not (member "work" tags))
            (should (member "home" tags)))))))

(ert-deftest org-glance-test:table-crypt-toggle ()
  "`#' encrypts the headline at point, then decrypts it: the stored blob and the
`encrypted?' projection flip each way."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO Alpha" "body"))
    (org-glance-test:with-table (graph)
        (let ((id (org-glance-test:first-row-id)))
          (org-glance-test:answering ((read-passwd "pw"))
            (org-glance-table--act-crypt graph id)          ; encrypt
            (should (org-glance-headline-metadata:encrypted?
                     (org-glance-graph:get-headline graph id)))
            (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph id)))
            (should (s-contains? "#+begin_crypt" (org-glance-graph:get-content graph id)))
            (should (equal "🔒" (org-glance-test:meta-cell graph id 'encrypted)))
            (org-glance-table--act-crypt graph id)          ; decrypt -> fully public
            (should-not (org-glance-headline-metadata:encrypted?
                         (org-glance-graph:get-headline graph id)))
            (should (s-contains? "body" (org-glance-graph:get-content graph id)))
            (should-not (s-contains? "#+begin_crypt" (org-glance-graph:get-content graph id)))
            (should (equal "" (org-glance-test:meta-cell graph id 'encrypted))))))))

(cl-defun org-glance-test:meta-cell (graph id key)
  "The KEY cell string of headline ID's `org-glance-table' row in GRAPH."
  (alist-get key (alist-get 'cells
                            (org-glance-table--row (org-glance-graph:get-headline graph id)))))

(ert-deftest org-glance-test:table-crypt-rekey ()
  "`C-u #' on an encrypted row re-keys it: it stays encrypted and decrypts with
the new password."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "c2" "* TODO Alpha" "body") "old"))
    (org-glance-test:with-table (graph)
        (let ((id (org-glance-test:first-row-id))
              (pws (list "old" "new")))
          (org-glance-test:answering ((read-passwd (pop pws)))
            (let ((current-prefix-arg '(4)))
              (org-glance-table--act-crypt graph id)))
          (should (org-glance-headline-metadata:encrypted?
                   (org-glance-graph:get-headline graph id)))
          (should (s-contains? "body" (org-glance-headline:contents
                                       (org-glance-headline:decrypt
                                        (org-glance-graph:headline graph id) "new"))))))))

;;; Per-view persistence (column order + sort)

(ert-deftest org-glance-test:table-persist-column-order ()
  "A column reorder is persisted per filter and restored on re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Alpha"))
    ;; first visit: default order has `state' first; move `title' to the front + persist
    (org-glance-test:with-table (graph)
        (should (equal "state" (alist-get 'key (car (alist-get 'columns table-view--spec)))))
        (setf (alist-get 'columns table-view--spec)
              (org-glance-table--reorder-columns (alist-get 'columns table-view--spec)
                                                 '("title" "state")))
        (org-glance-table--persist-config))
    ;; re-visit: restored order (`title' first)
    (org-glance-test:with-table (graph)
        (should (equal "title" (alist-get 'key (car (alist-get 'columns table-view--spec))))))))

(ert-deftest org-glance-test:table-persist-sort ()
  "A sort choice is persisted per filter and restored (applied) on re-visit."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "a" "* TODO Zeta")
                            (org-glance-test:headline "b" "* TODO Alpha"))
      ;; sort by title ascending + persist
      (org-glance-test:with-table (graph)
          (setq table-view--sort-keys '(("title" . t)))
          (org-glance-table--persist-config))
      ;; re-visit: sort restored AND applied (rows ordered by title: Alpha, Zeta)
      (org-glance-test:with-table (graph)
          (should (equal '(("title" . t)) table-view--sort-keys))
          (should (equal '("b" "a") (org-glance-test:row-ids table-view--rows)))))))

(ert-deftest org-glance-test:table-persist-per-filter ()
  "Configs are keyed per filter: one filter's layout does not leak to another."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "w" "* TODO A :work:")
                          (org-glance-test:headline "h" "* TODO B :home:"))
    (org-glance-test:with-table (graph 'work)
        (setf (alist-get 'columns table-view--spec)
              (org-glance-table--reorder-columns (alist-get 'columns table-view--spec)
                                                 '("tags" "state")))
        (org-glance-table--persist-config))
    ;; the :home filter still has the default order (state first)
    (org-glance-test:with-table (graph 'home)
        (should (equal "state" (alist-get 'key (car (alist-get 'columns table-view--spec))))))))

(ert-deftest org-glance-test:table-visit-default-directory ()
  "The table buffer's `default-directory' is the graph ROOT (matching the
corresponding overview), not wherever the non-file buffer was spawned."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO Alpha"))
    (org-glance-test:with-table (graph)
        (should (file-equal-p default-directory (org-glance-graph:directory graph))))))

(ert-deftest org-glance-test:table-renders-org-link-in-title ()
  "A headline title carrying an Org link renders as a followable description in
the table (`table-view' link support), not as raw `[[...]]' markup."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "k1" "* TODO Read [[https://example.com][The Book]]"))
    (let ((buf (org-glance-table:visit graph)))
      (unwind-protect
          (with-current-buffer buf
            (let ((txt (buffer-string)))
              (should (string-match-p "The Book" txt))            ; description shown
              (should-not (string-match-p "\\[\\[https" txt)))    ; raw markup hidden
            (goto-char (point-min))
            (should (re-search-forward "The Book" nil t))
            (should (equal "https://example.com"                   ; followable link property
                           (get-text-property (match-beginning 0) 'table-view-link)))
            (should (key-binding (kbd "C-c C-o"))))                ; follow key is bound
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest org-glance-test:table-fill-frame ()
  "With `org-glance-view-fill-frame' non-nil, visiting a table fills the frame
\(deletes the other windows); nil leaves the layout untouched."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "f1" "* TODO A"))
    (org-glance-test:assert-fills-frame (org-glance-table:visit graph))))

(ert-deftest org-glance-test:view-fill-frame-guards-undisplayed ()
  "`org-glance-view:fill-frame' is a no-op when the current buffer is not the one
shown in the selected window -- so it never deletes windows around an unrelated
buffer (a view opened programmatically, or a test that stubs the buffer switch)."
  (let ((org-glance-view-fill-frame t))
    (save-window-excursion
      (delete-other-windows) (split-window)             ; two windows
      (with-temp-buffer                                 ; current, but shown in NO window
        (org-glance-view:fill-frame)
        (should (= 2 (length (window-list))))))))        ; untouched

(ert-deftest org-glance-test:view-fill-frame-skips-when-in-view ()
  "`fill-frame' fills a fresh open but is a no-op when ALREADY-IN-VIEW, so
re-filtering / toggling from within a view keeps a deliberate split."
  (let ((org-glance-view-fill-frame t) (buf (generate-new-buffer " *ff-test*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer buf)                        ; buf shown in the selected window
          (split-window)
          (should (eq (window-buffer) buf))             ; guard precondition holds
          (let ((n (length (window-list))))
            (should (> n 1))                             ; the split produced >1 window
            (org-glance-view:fill-frame t)              ; re-navigation from a view -> no-op
            (should (= n (length (window-list))))        ; window count unchanged
            (org-glance-view:fill-frame nil)            ; a fresh open -> fills
            (should (= 1 (length (window-list))))))       ; down to one
      (kill-buffer buf))))

(ert-deftest org-glance-test:table-revisit-from-view-keeps-layout ()
  "Re-visiting the table from WITHIN a graph view keeps a deliberate split -- only
the first open (from outside a view) fills the frame."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "f1" "* TODO A"))
    (let ((org-glance-view-fill-frame t) (buf nil))
      (save-window-excursion
        (delete-other-windows)
        (setq buf (org-glance-table:visit graph))       ; fresh open from non-view -> fills
        (unwind-protect
            (progn
              (should (= 1 (length (window-list))))
              (split-window)                              ; user splits: 2 windows
              (with-current-buffer buf                    ; re-visit FROM within the view
                (org-glance-table:visit graph))           ; -> must NOT fill
              (should (= 2 (length (window-list)))))       ; split preserved
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest org-glance-test:view-fill-frame-survives-delete-error ()
  "A `delete-other-windows' signal never breaks opening the view -- caught even
under `debug-on-error' (so a quirky side/atomic window arrangement is harmless)."
  (let ((org-glance-view-fill-frame t) (debug-on-error t) reached)
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))              ; buffer IS the selected window's
        (cl-letf (((symbol-function 'delete-other-windows)
                   (lambda (&rest _) (setq reached t) (error "boom"))))
          (org-glance-view:fill-frame)                   ; must return, not signal
          (should reached))))))            ; reached the erroring delete, then swallowed it

(ert-deftest org-glance-test:table-visit-default-sort ()
  "The table opens sorted by the spec default (state, active-first) regardless of
graph insertion order -- guards `org-glance-table--apply-default-sort' against
`table-view''s seed-but-don't-apply default-sort semantics."
  (org-glance-test:with-graph graph
    ;; add DONE first, so load order (d, t) differs from the sorted order (t, d)
    (org-glance-graph:add graph
                             (org-glance-test:headline "d" "* DONE Zeta")
                             (org-glance-test:headline "t" "* TODO Alpha"))
    (org-glance-test:with-table (graph)
        (should (equal '("t" "d")
                       (org-glance-test:row-ids table-view--rows))))))

(ert-deftest org-glance-test:table-visit-tag-filter ()
  "org-glance-table:visit with a tag filter shows only matching headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "vt1" "* TODO Alpha :work:")
                             (org-glance-test:headline "vt2" "* TODO Beta :home:"))
    (org-glance-test:with-table (graph 'work)
        (should (= 1 (length table-view--rows)))
        (should (equal "vt1" (alist-get 'id (car table-view--rows)))))))

(ert-deftest org-glance-test:table-action-materialize ()
  "The materialize action handler opens via org-glance-material:open."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "am1" "* TODO Alpha"))
    (org-glance-test:with-shown (opened)
      (cl-letf (((symbol-function 'org-glance-material:open)
                 (lambda (_g id) (get-buffer-create (format "*mat-%s*" id)))))
        (org-glance-table--act-materialize graph "am1")
        (should (equal "*mat-am1*" (buffer-name opened)))))))   ; correct id opened

(ert-deftest org-glance-test:table-m-marks-not-materializes ()
  "In the table `m' toggles the row mark (`table-view-mark-toggle'), not
materialize (which stays on RET) -- org-glance no longer binds `m'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO Alpha")
                          (org-glance-test:headline "b" "* TODO Beta"))
    (org-glance-test:with-table (graph)
        (should (eq (key-binding (kbd "m")) #'table-view-mark-toggle))
        (should (eq (key-binding (kbd "U")) #'table-view-unmark-all))
        (should-not (eq (key-binding (kbd "RET")) #'table-view-mark-toggle))  ; RET is not the mark key
        (table-view--goto-id "a")
        (call-interactively (key-binding (kbd "m")))
        (should (member "a" table-view--marks)))))

(ert-deftest org-glance-test:table-todo-preserves-point ()
  "Changing state keeps point where it was instead of jumping to the top: when the
row leaves the view (DONE under an active filter) point stays on the same line."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "r1" "* TODO A")
                            (org-glance-test:headline "r2" "* TODO B")
                            (org-glance-test:headline "r3" "* TODO C"))
      (org-glance-test:with-table (graph '(:done nil))   ; active-only
          (should (table-view--goto-id "r2"))
          (let ((line (line-number-at-pos)))
            (should (> line 1))                     ; r2 is below the header
            (org-glance-table--act-todo graph "r2")   ; r2 -> DONE, leaves the view
            (should (= (line-number-at-pos) line))  ; point preserved, NOT at the top
            (should-not (table-view--goto-id "r2"))))))) ; r2 indeed gone

(ert-deftest org-glance-test:table-refresh-preserves-point ()
  "`g' (refresh) keeps point on the same row even when the sort reorders it -- the
re-fill + re-sort restore by line, so without re-anchoring point drifts."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "z1" "* TODO Zeta")
                            (org-glance-test:headline "a1" "* TODO Alpha"))
      (org-glance-test:with-table (graph)
          (setq table-view--sort-keys '(("title" . t)))   ; sort by title ascending
          (table-view-apply-sort)                          ; order: Alpha (a1), Zeta (z1)
          (should (table-view--goto-id "z1"))              ; z1 last sorted, first in load order
          (funcall (key-binding (kbd "g")))                ; refresh
          (should (equal "z1" (get-text-property (point) 'table-view-id)))))))  ; still on z1

(ert-deftest org-glance-test:table-action-todo ()
  "The `todo' action advances the row's state (`C-c C-t' via change-todo-live);
after the (no-note) commit the reloaded table shows the new state on the row."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "td1" "* TODO Alpha"))
      (org-glance-test:with-table (graph)
          (org-glance-table--act-todo graph "td1")   ; no-note -> synchronous finalize
          ;; persisted in the graph
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "td1"))))
          ;; reloaded row reflects the new state
          (let ((row (car table-view--rows)))
            (should (equal "td1" (alist-get 'id row)))
            (should (equal "DONE" (alist-get 'state (alist-get 'cells row)))))))))

(ert-deftest org-glance-test:table-bulk-state-prompt-is-org-native ()
  "The bulk prompt is org's own fast selection, initialized with the tag's
cycle -- the selector sees exactly the `#+TODO:' keywords `C-c C-t' would."
  (org-glance-test:with-graph graph
    (with-temp-directory cfg
      (let ((org-glance-tag-config-dir cfg)
            seen)
        (unwind-protect
            (progn
              (org-glance-test:write (f-join cfg "book.org")
                                     "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n* Book\n")
              (org-glance-tag-config--invalidate)
              (cl-letf (((symbol-function 'org-fast-todo-selection)
                         (lambda (&rest _) (setq seen org-todo-keywords-1) "READING")))
                (org-glance-table--read-state-native graph '(:tags ("book"))))
              ;; the temp selection buffer carried the TAG's cycle, not the global one
              (should (equal '("TODO" "READING" "READ") seen)))
          (org-glance-tag-config--invalidate))))))

(ert-deftest org-glance-test:table-bulk-todo-marked ()
  "`C-c C-t' with marked rows sets them all to a chosen state, then clears marks."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "b1" "* TODO A")
                            (org-glance-test:headline "b2" "* TODO B")
                            (org-glance-test:headline "b3" "* TODO C"))
      (org-glance-test:with-table (graph)
          (org-glance-test:mark-rows "b1" "b3")
          (should (= 2 (length (table-view-marked-rows))))
          (org-glance-test:answering ((org-fast-todo-selection "DONE"))
            (funcall (key-binding (kbd "C-c C-t"))))          ; bulk (marks present)
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "b1"))))
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "b3"))))
          (should (equal "TODO" (org-glance-headline-metadata:state  ; unmarked, untouched
                                 (org-glance-graph:get-headline graph "b2"))))
          (should (null (table-view-marked-rows)))))))       ; marks cleared

(ert-deftest org-glance-test:table-bulk-todo-preserves-point ()
  "After a bulk change, point stays on the row it was on, not jumping to the top."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "p1" "* TODO A")
                            (org-glance-test:headline "p2" "* TODO B")
                            (org-glance-test:headline "p3" "* TODO C")
                            (org-glance-test:headline "p4" "* TODO D"))
      (org-glance-test:with-table (graph)
          (org-glance-test:mark-rows "p1" "p2")
          (should (table-view--goto-id "p4"))            ; park point on an UNMARKED row
          (org-glance-test:answering ((org-fast-todo-selection "DONE"))
            (funcall (key-binding (kbd "C-c C-t"))))     ; bulk-sets p1,p2 (not p4)
          ;; point followed its row (p4), which survived the change
          (should (equal "p4" (get-text-property (point) 'table-view-id)))
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "p1"))))
          (should (equal "TODO" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "p4"))))))))

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
              (org-glance-test:mark-rows "g1" "g2")
              (table-view--goto-id "g3")                     ; point on the UNMARKED row
              (org-glance-test:answering ((org-fast-todo-selection "DONE"))
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
      (org-glance-test:with-table (graph)
          (should (table-view--goto-id "s1"))
          (should (null (table-view-marked-rows)))
          (funcall (key-binding (kbd "C-c C-t")))             ; single (no marks)
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "s1"))))))))

;;; Surgical single-row updates (buffer-text level)

(ert-deftest org-glance-test:table-schema-key-by-tags ()
  "The schema key is the filter's tags, sorted and joined; none -> \":none:\"."
  (should (equal "book" (org-glance-table--schema-key '(:tags ("book")))))
  (should (equal "article+book" (org-glance-table--schema-key '(:tags ("book" "article")))))
  (should (equal ":none:" (org-glance-table--schema-key nil))))

(ert-deftest org-glance-test:table-add-property-column-shows-drawer-value ()
  "An added property column displays each headline's drawer property."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "bk1" "* TODO The Hobbit :book:" '(("AUTHOR" . "Tolkien")))
      (org-glance-test:headline-props "bk2" "* TODO Dune :book:" '(("AUTHOR" . "Herbert"))))
    (org-glance-test:with-table (graph 'book)
      (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))
      (should (member "AUTHOR" (org-glance-test:table-col-keys)))
      (should (equal "Tolkien" (org-glance-test:table-cell "bk1" "AUTHOR")))
      (should (equal "Herbert" (org-glance-test:table-cell "bk2" "AUTHOR")))
      (should (string-match-p "Author"  (buffer-string)))
      (should (string-match-p "Tolkien" (buffer-string))))))

(ert-deftest org-glance-test:table-duplicate-row ()
  "`C-c p' adds a copy of the row's headline; the table shows both."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha :work:"))
    (org-glance-test:with-table (graph 'work)
      (table-view--goto-id "a")
      (org-glance-table--act-duplicate graph "a")
      (should (= 2 (length table-view--rows))))))

(ert-deftest org-glance-test:table-edit-cell ()
  "`i' edits the cell at point: state routes to the todo flow, title and
priority and property columns take a string prompt, derived columns refuse."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO [#B] Alpha :work:"
                                      '(("ROAST" . "light"))))
    (org-glance-test:with-table (graph 'work)
      (table-view-add-column (org-glance-table--property-column graph "ROAST"))
      ;; state column: routes to the todo flow
      (org-glance-test:goto-cell "a" "state")
      (let (todo)
        (cl-letf (((symbol-function 'org-glance-table--act-todo)
                   (lambda (_g id) (setq todo id))))
          (org-glance-table--act-edit graph "a"))
        (should (equal "a" todo)))
      ;; title column: string prompt, pre-filled
      (org-glance-test:goto-cell "a" "title")
      (let (offered)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_p &optional init &rest _) (setq offered init) "Beta")))
          (org-glance-table--act-edit graph "a"))
        (should (equal "Alpha" offered)))
      (should (equal "Beta" (org-glance-headline-metadata:title
                             (org-glance-graph:get-headline graph "a"))))
      ;; tags column: routes to the `:' tag flow
      (org-glance-test:goto-cell "a" "tags")
      (let (tagged)
        (cl-letf (((symbol-function 'org-glance-table--act-tag)
                   (lambda (_g id) (setq tagged id))))
          (org-glance-table--act-edit graph "a"))
        (should (equal "a" tagged)))
      ;; a truly derived column still refuses
      (org-glance-test:goto-cell "a" "repeated")
      (should-error (org-glance-table--act-edit graph "a") :type 'user-error)
      ;; priority column: empty input clears the cookie
      (org-glance-test:goto-cell "a" "priority")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
        (org-glance-table--act-edit graph "a"))
      (should-not (org-glance-headline-metadata:priority
                   (org-glance-graph:get-headline graph "a")))
      ;; drawer-property column: string prompt updates the drawer
      (org-glance-test:goto-cell "a" "ROAST")
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "dark")))
        (org-glance-table--act-edit graph "a"))
      (should (equal "dark" (org-glance-property-index:property graph "a" "ROAST"))))))

(ert-deftest org-glance-test:table-property-column-persists-per-tag ()
  "An added property column is saved per tag and restored (with values) on re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "bk1" "* TODO The Hobbit :book:" '(("AUTHOR" . "Tolkien"))))
    (org-glance-test:with-table (graph 'book)
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR")))
    ;; re-visit :book -> the AUTHOR column comes back, still reading the drawer
    (org-glance-test:with-table (graph 'book)
      (should (member "AUTHOR" (org-glance-test:table-col-keys)))
      (should (equal "Tolkien" (org-glance-test:table-cell "bk1" "AUTHOR"))))))

(ert-deftest org-glance-test:table-property-column-per-tag-isolation ()
  "A column added under one tag does not appear under another."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "bk1" "* TODO The Hobbit :book:" '(("AUTHOR" . "Tolkien")))
      (org-glance-test:headline-props "wk1" "* TODO Ship it :work:" '(("AUTHOR" . "Me"))))
    (org-glance-test:with-table (graph 'book)
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR")))
    (org-glance-test:with-table (graph 'work)
      (should-not (member "AUTHOR" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:table-remove-property-column ()
  "Removing an added column drops it, and the removal persists across a re-visit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "bk1" "* TODO The Hobbit :book:" '(("AUTHOR" . "Tolkien"))))
    (org-glance-test:with-table (graph 'book)
        (table-view-add-column (org-glance-table--property-column graph "AUTHOR"))
        (should (equal '(("AUTHOR" . "Author")) (org-glance-table--schema-get graph 'book)))
        (table-view-remove-column "AUTHOR")
        (should-not (org-glance-table--schema-get graph 'book)))
    (org-glance-test:with-table (graph 'book)
      (should-not (member "AUTHOR" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:table-cc-plus-adds-column ()
  "`C-c +' adds a property column; the bare `+' captures even with a prefix
arg, since columns moved off the `C-u' prefix onto `C-c +' / `C-c -'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "x1" "* TODO X :book:" '(("AUTHOR" . "Ann"))))
    (org-glance-test:with-table (graph 'book)
        (should (eq (key-binding (kbd "C-c +")) #'org-glance-table:add-column))
        (org-glance-test:answering ((completing-read "AUTHOR"))
          (funcall (key-binding (kbd "C-c +"))))
        (should (member "AUTHOR" (org-glance-test:table-col-keys)))
        (should (equal "Ann" (org-glance-test:table-cell "x1" "AUTHOR")))
        ;; `C-u +' no longer adds a column -- it captures like a bare `+'
        (let (captured)
          (cl-letf (((symbol-function 'org-glance-capture)
                     (lambda (&rest _) (setq captured t))))
            (let ((current-prefix-arg '(4))) (funcall (key-binding (kbd "+"))))
            (should captured))))))

;;; `-' untags; `C-c -' is the one column-removal key

(ert-deftest org-glance-test:table-minus-untags ()
  "A bare `-' always drops the view's tag -- with a prefix arg too, since
column removal moved to `C-c -'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "x1" "* TODO X :book:"))
    (org-glance-test:with-table (graph 'book)
        (let (called)
          (cl-letf (((symbol-function 'org-glance-table--act-deltag)
                     (lambda (&rest _) (setq called 'tag))))
            (let ((current-prefix-arg '(4))) (funcall (key-binding (kbd "-"))))
            (should (eq called 'tag))
            (setq called nil)
            (let ((current-prefix-arg nil)) (funcall (key-binding (kbd "-"))))
            (should (eq called 'tag)))))))

(ert-deftest org-glance-test:table-remove-column-at-point-title-fixed ()
  "`C-c -' on a column removes it; on Title it refuses (invariant 15)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "x1" "* TODO X :book:"))
    (org-glance-test:with-table (graph 'book)
        ;; point ON the Tags cell -> that column goes, no prompt
        (org-glance-test:goto-cell "x1" "tags")
        (org-glance-table:remove-column)
        (should-not (member "tags" (org-glance-test:table-col-keys)))
        ;; Title is refused (errors before touching the spec)
        (org-glance-test:goto-cell "x1" "title")
        (should-error (org-glance-table:remove-column) :type 'user-error)
        (should (member "title" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:table-builtin-removal-persists-per-tag ()
  "Removing a built-in column persists per tag: gone on reopen, other tags intact."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "bk1" "* TODO The Hobbit :book:")
      (org-glance-test:headline "ar1" "* TODO Essay :article:"))
    (org-glance-test:with-table (graph 'book)
        (should (member "tags" (org-glance-test:table-col-keys)))
        (table-view-remove-column "tags")
        (should (equal '("tags") (org-glance-table--schema-hidden graph 'book))))
    (org-glance-test:with-table (graph 'book)   ; reopen: still hidden
      (should-not (member "tags" (org-glance-test:table-col-keys))))
    (org-glance-test:with-table (graph 'article) ; other tag unaffected
      (should (member "tags" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:table-bare-minus-removes-view-tag ()
  "A bare `-' drops the view's tag off the headline at point: it leaves the view
but stays live in the graph (mirror of the bare `+' capture)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "bk1" "* TODO The Hobbit :book:"))
    (org-glance-test:with-table (graph 'book)
        (should (= 1 (length table-view--rows)))
        (org-glance-test:answering ((y-or-n-p t))
          (org-glance-table--act-deltag graph "bk1" '(:tags ("book"))))
        (should (null (org-glance-headline-metadata:tags        ; survives, untagged
                       (org-glance-graph:get-headline graph "bk1"))))
        (should (= 0 (length table-view--rows))))))            ; left the view

(ert-deftest org-glance-test:table-add-tag-excludes-own ()
  "Bare `:' offers only tags the headline does not already carry."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A :book:read:")
      (org-glance-test:headline "b" "* TODO B :film:"))   ; widens the tag universe
    (org-glance-test:with-table (graph 'book)
        (org-glance-test:offering (offered "")
          (org-glance-table--act-tag graph "a")          ; bare `:' (no prefix)
          (should (member "film" offered))                ; another headline's tag
          (should-not (member "book" offered))            ; own tags excluded
          (should-not (member "read" offered))))))

;;; Occurrence history: Rep column + `l' action

(ert-deftest org-glance-test:table-repeated-column-and-history ()
  "The Rep column marks repeater-carrying rows (↻ cell); `l' routes to the
shared picker."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "rep" "* TODO daily" "SCHEDULED: <2026-06-07 Sun +1d>")
      (org-glance-test:headline "one" "* TODO once"))
    (org-glance-test:with-table (graph)
        (should (equal "↻" (org-glance-test:table-cell "rep" "repeated")))
        (should (equal ""  (org-glance-test:table-cell "one" "repeated")))
        (let (picked)
          (cl-letf (((symbol-function 'org-glance-view:pick-occurrence)
                     (lambda (_g id) (setq picked id))))
            (table-view--goto-id "rep")
            (funcall (key-binding (kbd "l"))))
          (should (equal "rep" picked))))))

(ert-deftest org-glance-test:table-delete-key ()
  "`D' deletes the row's headline after confirmation and reloads the table."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "d1" "* TODO Doomed")
      (org-glance-test:headline "k1" "* TODO Keeper"))
    (org-glance-test:with-table (graph)
      (table-view--goto-id "d1")
      (org-glance-test:answering ((yes-or-no-p t))
        (funcall (key-binding (kbd "D"))))
      (should (equal '("k1") (org-glance-test:row-ids table-view--rows))))))

(ert-deftest org-glance-test:table-planning-keys ()
  "`C-c C-s' / `C-c C-d' set schedule/deadline on the row; `C-u' clears."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "p1" "* TODO Plan me"))
    (org-glance-test:with-table (graph)
        (table-view--goto-id "p1")
        (org-glance-test:answering ((org-read-date "2026-08-01"))
          (funcall (key-binding (kbd "C-c C-s"))))
        (should (s-contains? "2026-08-01"
                             (org-glance-headline-metadata:schedule
                              (org-glance-graph:get-headline graph "p1"))))
        (table-view--goto-id "p1")
        (org-glance-test:answering ((org-read-date "2026-09-01"))
          (funcall (key-binding (kbd "C-c C-d"))))
        (should (s-contains? "2026-09-01"
                             (org-glance-headline-metadata:deadline
                              (org-glance-graph:get-headline graph "p1"))))
        ;; C-u clears the schedule, deadline untouched
        (table-view--goto-id "p1")
        (let ((current-prefix-arg '(4)))
          (funcall (key-binding (kbd "C-c C-s"))))
        (let ((meta (org-glance-graph:get-headline graph "p1")))
          (should (null (org-glance-headline-metadata:schedule meta)))
          (should (s-contains? "2026-09-01" (org-glance-headline-metadata:deadline meta)))))))

;;; Scoped layout for reference tables (C-c C-c apply)

(cl-defun org-glance-test:ref-fixture (graph)
  "Two coffees each referencing one roaster."
  (org-glance-graph:add graph
    (org-glance-test:headline "c1" "* TODO Coffee1 :coffee:"
      "[[org-glance-material:r1][R1]]")
    (org-glance-test:headline "c2" "* TODO Coffee2 :coffee:"
      "[[org-glance-material:r2][R2]]")
    (org-glance-test:headline "r1" "* TODO Roaster1 :roaster:")
    (org-glance-test:headline "r2" "* TODO Roaster2 :roaster:")))

(ert-deftest org-glance-test:ref-layout-id-scope ()
  "Layout edits persist only on explicit apply; \"this headline\" scope keys on
the anchor id, so another anchor keeps the default layout."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    ;; edit WITHOUT apply -> the transient guards persist nothing
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (table-view-remove-column "tags")
      (org-glance-table--persist-schema)          ; what the live hooks would run
      (org-glance-table--persist-config))
    ;; nothing written anywhere: not the scoped store, not the per-tag/
    ;; per-filter stores the transient guards protect
    (should-not (file-exists-p (org-glance-table--refs-file graph)))
    (should-not (file-exists-p (org-glance-table--schema-file graph)))
    (should-not (file-exists-p (org-glance-table--config-file graph)))
    ;; edit + apply to the anchor headline; assert the offered scopes
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should (member "tags" (org-glance-test:table-col-keys)))   ; default restored
      (table-view-remove-column "tags")
      (org-glance-test:offering (coll (car coll))
        (org-glance-table:apply-layout)
        (should (equal coll '("this headline: Coffee1"
                              "tag pair: coffee → roaster")))))
    ;; same anchor restores; a different anchor does not
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should-not (member "tags" (org-glance-test:table-col-keys))))
    (org-glance-test:with-table (graph '(:id-any ("r2")) '(:anchor "c2" :dir relations))
      (should (member "tags" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:ref-layout-ignores-untagged-schema ()
  "A scope-less reference view shows default columns -- never the shared
untagged (\":none:\") per-tag schema entry."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    (org-glance-table--schema-put graph nil :hidden '("tags"))   ; the all-view entry
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should (member "tags" (org-glance-test:table-col-keys))))
    (org-glance-test:with-table (graph)                          ; all view honours it
      (should-not (member "tags" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:ref-layout-modified-nudge ()
  "A layout change in a reference view nudges (once per change) that
`C-c C-c' applies it; nothing is written."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (let (msgs)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
          (table-view-remove-column "tags")
          (org-glance-table--persist-config)
          (org-glance-table--persist-config))       ; unchanged -> no repeat
        (should (= 1 (cl-count-if (lambda (m) (s-contains? "C-c C-c" m)) msgs)))))
    (should-not (file-exists-p (org-glance-table--refs-file graph)))))

(ert-deftest org-glance-test:apply-layout-persistent-view ()
  "`C-c C-c' in a persistent (tag) view saves config + schema on the spot."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A :work:"))
    (org-glance-test:with-table (graph '(:tags ("work")))
      (should (eq (key-binding (kbd "C-c C-c")) #'org-glance-table:apply-layout))
      (table-view-remove-column "tags")
      (org-glance-table:apply-layout))
    (should (equal '("tags")
                   (org-glance-table--schema-hidden graph '(:tags ("work")))))))

(ert-deftest org-glance-test:ref-layout-hides-relation-column ()
  "Removing the relation view's own `Relation' column is persisted like any
built-in: `:hidden' diffs against the view's column set, which includes it, so
the removal survives `C-c C-c' instead of silently reappearing."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should (member "relation" (org-glance-test:table-col-keys)))
      (table-view-remove-column "relation")
      (org-glance-test:offering (coll (car coll))          ; "this headline: …"
        (org-glance-table:apply-layout)))
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (should-not (member "relation" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:ref-layout-pair-scope-and-tag-order ()
  "A tag-pair entry restores for ANY matching anchor, and never leaks into the
mirrored pair: the key is ANCHOR-tag > ROW-tag, so the same two tags viewed
from the other side is a different scope."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (table-view-remove-column "tags")
      (org-glance-test:offering (coll "tag pair: coffee → roaster")
        (org-glance-table:apply-layout)))
    ;; another coffee's relation table matches the pair
    (org-glance-test:with-table (graph '(:id-any ("r2")) '(:anchor "c2" :dir relations))
      (should-not (member "tags" (org-glance-test:table-col-keys))))
    ;; anchored on the roaster instead, the pair reads roaster > coffee -- untouched
    (org-glance-test:with-table (graph '(:id-any ("c1")) '(:anchor "r1" :dir relations))
      (should (member "tags" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:ref-layout-id-beats-pair ()
  "The anchor's own entry wins over a matching tag-pair entry."
  (org-glance-test:with-graph graph
    (org-glance-test:ref-fixture graph)
    (let ((file (org-glance-table--refs-file graph)))
      (org-glance--eld-alist-set file "pair:relations:coffee>roaster"
                                 '(:hidden ("tags") :applied 1))
      (org-glance--eld-alist-set file "ref:relations:c2"
                                 '(:hidden ("state") :applied 2)))
    (org-glance-test:with-table (graph '(:id-any ("r2")) '(:anchor "c2" :dir relations))
      (let ((keys (org-glance-test:table-col-keys)))
        (should (member "tags" keys))
        (should-not (member "state" keys))))))

(ert-deftest org-glance-test:ref-layout-latest-pair-wins ()
  "Among several matching pair entries the latest `:applied' wins."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "c3" "* TODO Coffee3 :coffee:decaf:"
        "[[org-glance-material:r1][R1]]")
      (org-glance-test:headline "r1" "* TODO Roaster1 :roaster:"))
    (let ((file (org-glance-table--refs-file graph)))
      (org-glance--eld-alist-set file "pair:relations:coffee>roaster"
                                 '(:hidden ("tags") :applied 1))
      (org-glance--eld-alist-set file "pair:relations:decaf>roaster"
                                 '(:hidden ("state") :applied 2)))
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c3" :dir relations))
      (let ((keys (org-glance-test:table-col-keys)))
        (should (member "tags" keys))
        (should-not (member "state" keys))))))

(ert-deftest org-glance-test:ref-layout-columns-order-sort-restore ()
  "A scoped entry restores custom columns, order and sort, exactly."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "c1" "* TODO Coffee1 :coffee:"
        "[[org-glance-material:r1][R1]]")
      (org-glance-test:headline-props "r1" "* TODO Roaster1 :roaster:"
                                      '(("ROAST" . "light"))))
    (org-glance--eld-alist-set
     (org-glance-table--refs-file graph) "ref:relations:c1"
     '(:columns (("ROAST" . "Roast")) :hidden ("tags")
       :order ("title" "state" "ROAST") :sort (("title" . t)) :applied 1))
    (org-glance-test:with-table (graph '(:id-any ("r1")) '(:anchor "c1" :dir relations))
      (let ((keys (org-glance-test:table-col-keys)))
        (should (equal '("title" "state") (cl-subseq keys 0 2)))   ; saved order
        (should (member "ROAST" keys))
        (should-not (member "tags" keys)))
      (should (equal '(("title" . t)) table-view--sort-keys))
      (should (equal "light" (org-glance-test:table-cell "r1" "ROAST"))))))

(ert-deftest org-glance-test:table-reload-keeps-cell ()
  "`g' (and every action's reload) returns point to the same CELL, not to
column 0.  (The row-left-the-view fallback is `table-todo-preserves-point'.)"
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO Alpha :work:")
      (org-glance-test:headline "b" "* TODO Beta :work:"))
    (org-glance-test:with-table (graph 'work)
      (org-glance-test:goto-cell "b" "tags")
      (let ((col (current-column)))
        (org-glance-table--reload (current-buffer))
        (should (equal "b" (get-text-property (point) 'table-view-id)))
        (should (equal "tags" (get-text-property (point) 'table-view-col)))
        (should (= col (current-column)))))))

(ert-deftest org-glance-test:table-configure-tag ()
  "`C' edits the table's sole tag directly; multi/no tag re-prompts (nil)."
  (let (captured)
    (cl-letf (((symbol-function 'org-glance-tag-config-edit)
               (lambda (&optional tag) (push tag captured))))
      (let ((org-glance-table--spec '(:tags ("work"))))
        (org-glance-table:configure-tag))
      (let ((org-glance-table--spec nil))
        (org-glance-table:configure-tag)))
    (should (equal '(nil "work") captured))))

(ert-deftest org-glance-test:table-action-upserts-one-row ()
  "A single-row action updates THAT row from fresh metadata instead of
re-deriving every row from the graph, and drops the row when the headline
leaves the view (deleted, or no longer matching the filter)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a1" "* TODO Alpha :work:")
      (org-glance-test:headline "b1" "* TODO Beta :work:"))
    (org-glance-test:with-table (graph '(:tags ("work")))
      (let ((derivations 0))
        (cl-letf* ((orig (symbol-function 'org-glance-table--rows))
                   ((symbol-function 'org-glance-table--rows)
                    (lambda (&rest args) (cl-incf derivations) (apply orig args))))
          ;; the store advances, then the action finishes on that one row
          (org-glance-graph:add graph (org-glance-test:headline "a1" "* DONE Alpha :work:"))
          (org-glance-table--finish "a1" 1 "State: %s" "DONE")
          (should (= 0 derivations))                     ; no whole-set rebuild
          (should (equal "DONE" (org-glance-test:table-cell "a1" "state")))
          (should (equal "TODO" (org-glance-test:table-cell "b1" "state")))
          ;; leaving the filter drops the row (here: the tag goes away)
          (org-glance-graph:add graph (org-glance-test:headline "a1" "* DONE Alpha"))
          (org-glance-table--finish "a1" 1 "Removed tag")
          (should (= 0 derivations))
          (should (equal '("b1") (mapcar (lambda (r) (alist-get 'id r)) table-view--rows)))
          ;; a deleted headline drops too
          (org-glance-graph:delete graph "b1")
          (org-glance-table--finish "b1" 1 "Headline deleted")
          (should (= 0 derivations))
          (should (null table-view--rows)))))))

(provide 'test-table)
;;; test-table.el ends here
