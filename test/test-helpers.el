;;; test-helpers.el --- Helpers for org-glance
(require 'ert)
(require 's)
(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Create a temporary directory, bind it to DIR, run BODY in it, and delete the directory afterward.
DIR is a symbol that will hold the path to the temporary directory within BODY."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

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
  "Build an `org-glance-headline' carrying ID.
LINES is the heading, then optional planning (SCHEDULED:/DEADLINE:/CLOSED:)
lines, then optional body.  The ORG_GLANCE_ID drawer is placed after the
heading and any planning lines -- where org expects a property drawer --
so the id parses correctly whether or not a body or planning is present."
  (apply #'org-glance-test:headline-props id (car lines) nil (cdr lines)))

(cl-defun org-glance-test:change-todo-live (graph id &optional arg)
  "Run `org-glance-material:change-todo-live' (the no-note path) in a live origin
buffer and return the finalized state string.  The no-note commit is synchronous,
so no timer pumping is needed."
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
Negative backdates it (existing caches read as fresh); positive advances
it (caches read as stale)."
  (set-file-times (org-glance-graph:headline-meta-path graph)
                  (time-add (current-time) seconds)))

(cl-defun org-glance-test:open-size (graph)
  "Byte size of GRAPH's open (unsealed) segment file, 0 if absent."
  (or (file-attribute-size
       (file-attributes (org-glance-graph:headline-meta-path graph)))
      0))

(cl-defun org-glance-test:sed (from to)
  "From `point-min', find regexp FROM in the current buffer, replace with TO."
  (goto-char (point-min))
  (re-search-forward from)
  (replace-match to))

(cl-defun org-glance-test:org-with-id (heading id &optional body)
  "Return org text: HEADING, ORG_GLANCE_ID drawer for ID, then optional BODY."
  (format "%s\n:PROPERTIES:\n:ORG_GLANCE_ID: %s\n:END:\n%s"
          heading id (if body (concat body "\n") "")))

(cl-defmacro org-glance-test:with-material ((buffer graph id) &rest body)
  "Materialize ID from GRAPH into BUFFER, make it current, run BODY, kill it.
The modified flag is cleared and the buffer unconditionally killed on exit."
  (declare (indent 1))
  `(let ((,buffer (org-glance-material:open ,graph ,id)))
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
  "Visit GRAPH's overview for FILTER, bind the buffer to BUF for BODY,
kill it afterward.  Dynamically binds `org-glance-graph' to GRAPH."
  (declare (indent 1))
  `(let* ((org-glance-graph ,graph)
          (,buf (org-glance-overview:visit ,graph ,filter)))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p ,buf)
         (with-current-buffer ,buf (set-buffer-modified-p nil))
         (kill-buffer ,buf)))))

(cl-defmacro org-glance-test:counting-renders ((counter &optional (return "")) &rest body)
  "Bind COUNTER to 0 and stub `org-glance-overview:render' to bump it and
return RETURN, then run BODY."
  (declare (indent 1))
  `(let ((,counter 0))
     (cl-letf (((symbol-function 'org-glance-overview:render)
                (lambda (&rest _) (cl-incf ,counter) ,return)))
       ,@body)))

(cl-defmacro org-glance-test:with-failing-ingest (id &rest body)
  "Run BODY with `org-glance-graph:add' erroring on any headline with id ID.
All other ingests proceed through the real function."
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
  "Visit GRAPH's table for FILTER (reference CONTEXT or nil), bind the buffer
to VAR, run BODY, kill it.  Implemented over `org-glance-test:with-shown'
\(same show-stubs + kill-all)."
  (declare (indent 4))
  `(org-glance-test:with-shown (,var)
     (setq ,var (org-glance-table:visit ,graph ,filter :context ,context))
     ,@body))

(cl-defmacro org-glance-test:with-table ((graph &optional filter context) &rest body)
  "Visit GRAPH's table for FILTER (reference-view CONTEXT or nil); run BODY
with the table buffer current, kill it afterward.  BODY refers to the buffer
via `current-buffer'."
  (declare (indent 1) (debug ((form &optional form form) body)))
  (let ((buf (gensym "table-buf")))
    `(org-glance-test:with-table-filter ,graph ,filter ,buf ,context
       (with-current-buffer ,buf ,@body))))

(cl-defun org-glance-test:table-col-keys (&optional (buf (current-buffer)))
  "Display-order column keys of table BUF (default: current buffer)."
  (with-current-buffer buf
    (mapcar (lambda (c) (alist-get 'key c)) (table-view--columns table-view--spec))))

(cl-defun org-glance-test:table-cell (id key &optional (buf (current-buffer)))
  "Cell KEY of row ID in table BUF (default: current buffer)."
  (with-current-buffer buf
    (table-view--cell (cl-find id table-view--rows
                               :key (lambda (r) (alist-get 'id r)) :test #'equal)
                      key)))

(cl-defmacro org-glance-test:offering ((coll answer) &rest body)
  "Stub `completing-read' around BODY, recording what it offers in COLL.
Each prompt stores its COLLECTION argument in COLL (last prompt wins), then
replies with ANSWER, a form that may read COLL (e.g. `(caar coll)').  COLL
stays bound around BODY for assertions on what was offered.  Complement of
`org-glance-test:answering', whose stubs cannot see their arguments."
  (declare (indent 1) (debug ((symbolp form) body)))
  `(let (,coll)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_p c &rest _) (setq ,coll c) ,answer)))
       ,@body)))

(cl-defmacro org-glance-test:with-shown ((var) &rest body)
  "Stub `switch-to-buffer'/`pop-to-buffer' to record shown buffers in VAR
\(most recent; the stubs still return the buffer).  Run BODY; on exit KILL
every buffer shown, modified flags cleared -- shown buffers must never leak
into later tests (`org-glance-llm--label' scans live buffers)."
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
  "Build a headline: HEADING, planning lines from BODY's head, the drawer
\(ORG_GLANCE_ID + PROPS as (KEY . VALUE) pairs), then the rest of BODY.
Planning lines (SCHEDULED:/DEADLINE:/CLOSED:) are hoisted BEFORE the drawer,
where org expects them."
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
  "Stub prompting functions to constant answers around BODY.
BINDINGS is ((FN VALUE)...); each FN becomes a lambda ignoring its args and
returning VALUE.  For prompts whose stub must inspect its arguments, use a
plain `cl-letf'."
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
  "Sorted headline ids of a picker's CANDIDATES alist (label . metadata)."
  (sort (mapcar (lambda (c) (org-glance-headline-metadata:id (cdr c))) candidates)
        #'string<))

(defun org-glance-test:row-ids (rows)
  "The `id' cell of each row in ROWS."
  (mapcar (lambda (r) (alist-get 'id r)) rows))

(cl-defmacro org-glance-test:assert-fills-frame (visit-form)
  "Assert VISIT-FORM fills the frame when `org-glance-view-fill-frame' is non-nil
and leaves a two-window split alone when nil.  VISIT-FORM must return the view
buffer and really display it (`switch-to-buffer')."
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
  "HEADLINE re-parsed with its whole body as legacy (pre-block) ciphertext.
Fabricates the pre-crypt-block on-disk layout for upgrade/compat tests."
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
