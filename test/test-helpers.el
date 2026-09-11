;;; test-helpers.el --- Helpers for org-glance
(require 'ert)
(require 's)
(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Bind symbol DIR to a fresh temporary directory, run BODY, then delete it."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(cl-defun org-glance-test:save ()
  "Save the current buffer without its echo-area noise."
  (let ((inhibit-message t)) (save-buffer)))

(cl-defun org-glance-test:write (path text)
  "Write TEXT to PATH, creating parent directories."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text text 'utf-8 path))

(cl-defun org-glance-test:conflict-open (ours theirs)
  "Return text git left conflict-marked with OURS over THEIRS.
OURS and THEIRS are newline-terminated line strings; the sole marker speller."
  (concat "<<<<<<< HEAD\n" ours "=======\n" theirs ">>>>>>> other-machine\n"))

(cl-defmacro org-glance-test:session (&rest body)
  (declare (indent 0))
  `(with-temp-directory org-glance-directory
     (org-glance-init org-glance-directory)
     ,@body))

(cl-defmacro org-glance-test:with-graph (graph &rest body)
  "Create a graph in a fresh temp directory, bind it to GRAPH, run BODY."
  (declare (indent 1))
  `(with-temp-directory dir
     (let ((,graph (org-glance-graph dir)))
       ,@body)))

(cl-defun org-glance-test:headline (id &rest lines)
  "Build an `org-glance-headline' carrying ID from LINES.
LINES is heading, planning, then body lines; the id drawer follows planning."
  (apply #'org-glance-test:headline-props id (car lines) nil (cdr lines)))

(cl-defun org-glance-test:change-todo-live (graph id &optional arg)
  "Run `org-glance-material:change-todo-live' on GRAPH's ID with ARG.
Return the state its synchronous no-note path finalizes, or nil."
  (let ((origin (generate-new-buffer " *ctl-origin*"))
        (result 'unset))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (org-glance-material:change-todo-live
             graph id arg (lambda (state) (setq result state))))
          (unless (eq result 'unset) result))
      (kill-buffer origin))))

(cl-defun org-glance-test:capture (graph text)
  "Capture TEXT (org source) into GRAPH via a temp buffer."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (org-glance-graph:capture graph (current-buffer))))

(defun org-glance-test:ids (graph)
  "Return the ids of GRAPH's live headlines, in first-sighting order."
  (mapcar #'org-glance-headline-metadata:id (org-glance-graph:headlines graph)))

(cl-defun org-glance-test:filter-ids (graph filter)
  "Return the ids of GRAPH's headlines matching FILTER, in graph order."
  (mapcar #'org-glance-headline-metadata:id
          (seq-filter (org-glance-filter:predicate filter)
                      (org-glance-graph:headlines graph))))

(cl-defun org-glance-test:store-mtime (graph seconds)
  "Set GRAPH's headline-meta mtime SECONDS relative to now.
Negative SECONDS makes existing caches read fresh; positive, stale."
  (set-file-times (org-glance-graph:headline-meta-path graph)
                  (time-add (current-time) seconds)))

(cl-defun org-glance-test:sealed-segments (graph)
  "Return the names of the sealed segments GRAPH's MANIFEST lists."
  (org-glance-graph--sealed-segments graph))

(cl-defun org-glance-test:count-records (graph)
  "Return how many records a forward scan of GRAPH's store yields."
  (let ((n 0))
    (org-glance-graph--scan-forward graph (lambda (_r) (cl-incf n)))
    n))

(cl-defmacro org-glance-test:with-crash-at (fn &rest body)
  "Run BODY with FN signalling, simulating a crash at that commit point."
  (declare (indent 1))
  `(cl-letf (((symbol-function ,fn)
              (lambda (&rest _) (error "simulated crash at commit"))))
     ,@body))

(cl-defun org-glance-test:simulate-material-save (graph id contents)
  "Write CONTENTS to ID's blob in GRAPH, then run `org-glance-material:sync'."
  (let ((path (org-glance-graph:content-path graph id)))
    (org-glance-test:write path contents)
    (with-temp-buffer
      (setq-local org-glance-material--graph graph
                  org-glance-material--id id)
      (let ((buffer-file-name path))
        (org-glance-material:sync)))))

(cl-defun org-glance-test:open-size (graph)
  "Return the byte size of GRAPH's open segment file, or 0 if absent."
  (org-glance--file-size (org-glance-graph:headline-meta-path graph)))

(cl-defun org-glance-test:sed (from to)
  "From `point-min', find regexp FROM in the current buffer, replace with TO."
  (goto-char (point-min))
  (re-search-forward from)
  (replace-match to))

(cl-defun org-glance-test:org-with-id (heading id &optional body)
  "Return org text: HEADING, ORG_GLANCE_ID drawer for ID, then optional BODY."
  (format "%s\n:PROPERTIES:\n:ORG_GLANCE_ID: %s\n:END:\n%s"
          heading id (if body (concat body "\n") "")))

(cl-defmacro org-glance-test:with-material ((buffer graph id &rest opts) &rest body)
  "Materialize ID from GRAPH into BUFFER and run BODY there.
OPTS pass to `org-glance-material:open'; exit always kills BUFFER unmodified."
  (declare (indent 1))
  `(let ((,buffer (org-glance-material:open ,graph ,id ,@opts)))
     (unwind-protect
         (with-current-buffer ,buffer ,@body)
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer (set-buffer-modified-p nil))
         (kill-buffer ,buffer)))))

(cl-defmacro org-glance-test:with-note-origin ((origin) &rest body)
  "Bind ORIGIN to a fresh origin buffer; kill it and any *Org Note* after BODY."
  (declare (indent 1))
  `(let ((,origin (generate-new-buffer " *note-origin*")))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,origin) (kill-buffer ,origin))
       (when (get-buffer "*Org Note*") (kill-buffer "*Org Note*")))))

(cl-defmacro org-glance-test:with-overview ((buf graph filter) &rest body)
  "Visit GRAPH's overview for FILTER as BUF around BODY, then kill it.
Binds `org-glance-graph' to GRAPH dynamically."
  (declare (indent 1))
  `(let* ((org-glance-graph ,graph)
          (,buf (org-glance-overview:visit ,graph ,filter)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,buf)
         (with-current-buffer ,buf (set-buffer-modified-p nil))
         (kill-buffer ,buf)))))

(cl-defmacro org-glance-test:counting-renders ((counter &optional (return "")) &rest body)
  "Run BODY with `org-glance-overview:render' stubbed to count into COUNTER.
COUNTER starts at 0; the stub returns RETURN."
  (declare (indent 1))
  `(let ((,counter 0))
     (cl-letf (((symbol-function 'org-glance-overview:render)
                (lambda (&rest _) (cl-incf ,counter) ,return)))
       ,@body)))

(cl-defmacro org-glance-test:with-failing-ingest (id &rest body)
  "Run BODY with `org-glance-graph:add' erroring only on calls that add ID."
  (declare (indent 1))
  `(let ((orig (symbol-function 'org-glance-graph:add)))
     (cl-letf (((symbol-function 'org-glance-graph:add)
                (lambda (graph &rest headlines)
                  (if (cl-some (lambda (h) (string= ,id (org-glance-headline:id h)))
                               headlines)
                      (error "simulated ingest failure")
                    (apply orig graph headlines)))))
       ,@body)))

(cl-defmacro org-glance-test:with-open (buf open-form &rest body)
  "Bind BUF to OPEN-FORM, run BODY, then kill BUF if still live."
  (declare (indent 2))
  `(let ((,buf ,open-form))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,buf) (kill-buffer ,buf)))))

(cl-defmacro org-glance-test:with-table-filter (graph filter var context &rest body)
  "Bind VAR to GRAPH's table for FILTER and CONTEXT around BODY, then kill it.
CONTEXT is a reference context or nil; built on `org-glance-test:with-shown'."
  (declare (indent 4))
  `(org-glance-test:with-shown (,var)
     (setq ,var (org-glance-table:visit ,graph ,filter :context ,context))
     ,@body))

(cl-defmacro org-glance-test:with-table ((graph &optional filter context) &rest body)
  "Run BODY in GRAPH's table for FILTER and reference CONTEXT, then kill it."
  (declare (indent 1) (debug ((form &optional form form) body)))
  (let ((buf (gensym "table-buf")))
    `(org-glance-test:with-table-filter ,graph ,filter ,buf ,context
       (with-current-buffer ,buf ,@body))))

(cl-defun org-glance-test:table-col-keys (&optional (buf (current-buffer)))
  "Return the display-order column keys of table BUF, default current buffer."
  (with-current-buffer buf
    (mapcar (lambda (c) (alist-get 'key c)) (table-view--columns table-view--spec))))

(cl-defun org-glance-test:goto-cell (id key)
  "Move point to row ID's KEY cell in the current table and assert its column."
  (table-view--goto-id id)
  (table-view--goto-cell key)
  (should (equal key (get-text-property (point) 'table-view-col))))

(cl-defun org-glance-test:table-cell (id key &optional (buf (current-buffer)))
  "Return cell KEY of row ID in table BUF, default current buffer."
  (with-current-buffer buf
    (table-view--cell (cl-find id table-view--rows
                               :key (lambda (r) (alist-get 'id r)) :test #'equal)
                      key)))

(cl-defmacro org-glance-test:offering ((coll answer) &rest body)
  "Stub `completing-read' around BODY, recording the last collection in COLL.
Each prompt returns ANSWER, a form that may read COLL, e.g. `(caar coll)'.
Complement of `org-glance-test:answering', whose stubs cannot see arguments."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(let (,coll)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_p c &rest _) (setq ,coll c) ,answer)))
       ,@body)))

(cl-defmacro org-glance-test:with-shown ((var) &rest body)
  "Run BODY with `switch-to-buffer'/`pop-to-buffer' stubbed to set VAR.
VAR holds the last buffer shown, which the stubs still return.  Exit kills
every shown buffer, modified flags cleared, so none leaks into later tests."
  (declare (indent 1) (debug ((symbolp) body)))
  (let ((all (gensym "shown-all")))
    `(let (,var ,all)
       (unwind-protect
           (cl-letf (((symbol-function 'switch-to-buffer)
                      (lambda (b &rest _) (push b ,all) (setq ,var b) b))
                     ((symbol-function 'pop-to-buffer)
                      (lambda (b &rest _) (push b ,all) (setq ,var b) b)))
             ,@body)
         (dolist (b ,all)
           (when (buffer-live-p b)
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))))))

(cl-defun org-glance-test:headline-props (id heading props &rest body)
  "Build a headline carrying ID from HEADING, PROPS and BODY.
BODY's leading SCHEDULED:/DEADLINE:/CLOSED: lines follow HEADING, then a
drawer of ORG_GLANCE_ID and the (KEY . VALUE) PROPS, then the rest of BODY."
  (let* ((planning (seq-take-while
                    (lambda (l) (string-match-p "^\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" l))
                    body))
         (rest (seq-drop body (length planning))))
    (apply #'org-glance-headline--from-lines
           (append (list heading) planning
                   (list ":PROPERTIES:" (format ":ORG_GLANCE_ID: %s" id))
                   (mapcar (lambda (kv) (format ":%s: %s" (car kv) (cdr kv))) props)
                   (list ":END:")
                   rest))))

(defun org-glance-test:first-row-id ()
  "Move point to the current table's first data row and return its id."
  (goto-char (point-min))
  (table-view--goto-first-row)
  (get-text-property (point) 'table-view-id))

(defun org-glance-test:mark-rows (&rest ids)
  "Toggle the table mark on each row in IDS."
  (dolist (id ids)
    (table-view--goto-id id)
    (call-interactively #'table-view-mark-toggle)))

(cl-defmacro org-glance-test:answering (bindings &rest body)
  "Stub each FN of BINDINGS ((FN VALUE)...) to a constant VALUE around BODY."
  (declare (indent 1))
  `(cl-letf ,(mapcar (lambda (b)
                       `((symbol-function ',(car b)) (lambda (&rest _) ,(cadr b))))
                     bindings)
     ,@body))

(cl-defmacro org-glance-test:with-repeat ((buf graph id depth) &rest body)
  "Materialize ID from GRAPH with repeat-history DEPTH and quiet repeat logs."
  (declare (indent 1))
  `(let ((org-glance-repeat-history-depth ,depth)
         (org-log-repeat nil)
         (org-log-done nil))
     (org-glance-test:with-material (,buf ,graph ,id) ,@body)))

(defun org-glance-test:complete-repetition ()
  "Complete the current repetition (state to DONE; org advances the repeater)."
  (goto-char (point-min))
  (org-todo "DONE"))

(defun org-glance-test:offered-ids (candidates)
  "Return the sorted headline ids of a picker's (label . metadata) CANDIDATES."
  (sort (mapcar (lambda (c) (org-glance-headline-metadata:id (cdr c))) candidates)
        #'string<))

(cl-defmacro org-glance-test:field (graph id field)
  "Return GRAPH's stored FIELD for ID; FIELD is an unquoted metadata slot name."
  `(,(intern (format "org-glance-headline-metadata:%s" field))
    (org-glance-graph:get-headline ,graph ,id)))

(cl-defmacro org-glance-test:with-todo-done (&rest body)
  "Run BODY under a plain TODO/DONE cycle with done-logging off."
  (declare (indent 0))
  `(let ((org-todo-keywords '((sequence "TODO" "DONE")))
         (org-log-done nil))
     ,@body))

(cl-defmacro org-glance-test:with-seal-each-insert (&rest body)
  "Run BODY with every insert sealing its segment and auto-compaction off."
  (declare (indent 0))
  `(let ((org-glance-graph-segment-max-bytes 1)
         (org-glance-graph-compact-segment-count 1000))
     ,@body))

(cl-defun org-glance-test:meta-cell (graph id key)
  "Return the KEY cell of ID's `org-glance-table' row derived fresh from GRAPH.
`org-glance-test:table-cell' reads a table buffer's row instead."
  (alist-get key (alist-get 'cells
                            (org-glance-table--row (org-glance-graph:get-headline graph id)))))

(defun org-glance-test:row-ids (rows)
  "Return the `id' cell of each row in ROWS."
  (mapcar (lambda (r) (alist-get 'id r)) rows))

(cl-defmacro org-glance-test:assert-fills-frame (visit-form)
  "Assert VISIT-FORM fills the frame iff `org-glance-view-fill-frame' is set.
VISIT-FORM must return the view buffer, displayed via `switch-to-buffer'."
  `(save-window-excursion
     (dolist (case '((t . 1) (nil . 2)))
       (delete-other-windows) (split-window)          ; two windows before the visit
       (let* ((org-glance-view-fill-frame (car case))
              (buf ,visit-form))
         (unwind-protect
             (progn
               (should (eq (window-buffer) buf))       ; the view is what's shown
               (should (= (cdr case) (length (window-list)))))
           (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun org-glance-test:reopen (graph)
  "Drop GRAPH from the instance cache and re-open it (running heal/migration)."
  (let ((dir (org-glance-graph:directory graph)))
    (remhash dir org-glance-graph:list)
    (org-glance-graph dir)))

(cl-defun org-glance-test:legacy-encrypt (headline password)
  "Return HEADLINE re-parsed with its whole body as legacy PASSWORD ciphertext."
  (org-glance-headline--from-string
   (org-glance-headline:with-contents headline
     (org-end-of-meta-data t)
     (let* ((beg (point))
            (cipher (aes-encrypt-buffer-or-string
                     (buffer-substring-no-properties beg (point-max)) password)))
       (delete-region beg (point-max))
       (insert cipher))
     (buffer-string))))

(provide 'test-helpers)
;;; test-helpers.el ends here
