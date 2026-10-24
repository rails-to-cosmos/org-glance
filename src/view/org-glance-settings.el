;;; org-glance-settings.el --- Shared JSON settings tables -*- lexical-binding: t -*-

(require 'org-glance-config)
(require 'org-glance-view)
(require 'org-glance-core)
(require 'table-view)

(cl-defstruct (org-glance-settings-draft (:constructor org-glance-settings--draft))
  graph tag revision base values error)

(defvar-local org-glance-settings--session nil)
(defvar-local org-glance-settings--page nil)
(defvar-local org-glance-settings--parent nil)

(defun org-glance-settings--choose (prompt choices &optional initial)
  "Require one of CHOICES for PROMPT, starting at INITIAL."
  (let ((value (completing-read prompt choices nil t initial)))
    (unless (member value choices) (user-error "Choose one of: %s" (string-join choices ", ")))
    value))

(defun org-glance-settings--copy (value)
  "Copy a decoded JSON VALUE."
  (json-parse-string (json-serialize value :null-object :null :false-object :false)
                     :null-object :null :false-object :false))

(defun org-glance-settings--scope (snapshot tag)
  "Return SNAPSHOT's known overrides for TAG, nil for system."
  (let* ((object (org-glance-config-file:object snapshot))
         (entry (if tag
                    (cdr (cl-find tag (org-glance-config--pairs (or (gethash "tags" object)
                                                                  (make-hash-table :test 'equal)))
                                  :key (lambda (pair) (org-glance-config--tag-name (car pair)))
                                  :test #'equal)) object))
         (values (make-hash-table :test 'equal)))
    (dolist (field (org-glance-config:workflow-fields tag))
      (unless (or (null entry) (eq (gethash field entry :missing) :missing))
        (puthash field (gethash field entry) values)))
    (org-glance-settings--copy values)))

(defun org-glance-settings--observe (graph tag)
  "Prepare a validated settings draft for GRAPH and TAG."
  (let ((snapshot (org-glance-config-file:read (org-glance-graph:directory graph))))
    (unless (org-glance-config-file:revision snapshot)
      (user-error "Settings tables require a published config.json"))
    (org-glance-config:decode snapshot)
    (let ((base (org-glance-settings--scope snapshot tag)))
      (org-glance-settings--draft :graph graph :tag tag
                                 :revision (org-glance-config-file:revision snapshot)
                                 :base base :values (org-glance-settings--copy base)))))

(defun org-glance-settings--dirty (session)
  "Whether SESSION has an unacknowledged draft or failed live apply."
  (or (org-glance-settings-draft-error session)
      (not (org-glance-config-file--equal (org-glance-settings-draft-base session)
                                         (org-glance-settings-draft-values session)))))

(defun org-glance-settings--lines (text)
  "Split TEXT into (TEXT . ENDING) pairs without losing line endings."
  (let ((pos 0) rows)
    (while (< pos (length text))
      (if (string-match "[\r\n]" text pos)
          (let* ((end (match-beginning 0))
                 (next (+ end (if (and (= (aref text end) ?\r)
                                       (< (1+ end) (length text))
                                       (= (aref text (1+ end)) ?\n)) 2 1))))
            (push (cons (substring text pos end) (substring text end next)) rows)
            (setq pos next))
        (push (cons (substring text pos) "") rows)
        (setq pos (length text))))
    (nreverse rows)))

(defconst org-glance-settings--endings '(("LF" . "\n") ("CRLF" . "\r\n") ("CR" . "\r") ("none" . "")))

(defun org-glance-settings--items ()
  "Return the current child table's ordered draft items."
  (let ((value (gethash org-glance-settings--page
                        (org-glance-settings-draft-values org-glance-settings--session))))
    (if (equal org-glance-settings--page "todoCycles") (append value nil)
      (org-glance-settings--lines (or value "")))))

(defun org-glance-settings--set-items (items)
  "Replace the child table's draft with ordered ITEMS."
  (when (and (equal org-glance-settings--page "template")
             (cl-some (lambda (line) (equal (cdr line) "")) (butlast items)))
    (user-error "Only the last template line can have no ending; choose an ending first"))
  (puthash org-glance-settings--page
           (if (equal org-glance-settings--page "todoCycles") (vconcat items)
             (mapconcat (lambda (line) (concat (car line) (cdr line))) items ""))
           (org-glance-settings-draft-values org-glance-settings--session)))

(defun org-glance-settings--row (id &rest cells)
  "Construct a table row with ID and CELLS."
  `((id . ,id) (cells . ,cells)))

(defun org-glance-settings--state ()
  "Return the current draft's publication state."
  (cond ((org-glance-settings-draft-error org-glance-settings--session) "error")
        ((org-glance-settings--dirty org-glance-settings--session) "changed")
        (t "saved")))

(defun org-glance-settings--rows ()
  "Project the current scope or child table."
  (let* ((session org-glance-settings--session)
         (values (org-glance-settings-draft-values session))
         (state (org-glance-settings--state)))
    (if (null org-glance-settings--page)
        (mapcar
         (lambda (field)
           (let ((value (gethash field values :missing)))
             (org-glance-settings--row
              field (cons 'setting (cdr (assoc field '(("title" . "Title") ("todoCycles" . "TODO cycles")
                                                       ("template" . "Capture template") ("storage" . "Storage")))))
              (cons 'value (if (eq value :missing) "inherited / production default"
                             (if (member field '("todoCycles" "template")) "open table" value)))
              (cons 'area (cdr (assoc field '(("title" . "Tags") ("todoCycles" . "Keywords")
                                              ("template" . "Capture") ("storage" . "Storage")))))
              (cons 'applies (if (org-glance-settings-draft-tag session)
                                (concat "tag:" (org-glance-settings-draft-tag session)) "whole tree"))
              (cons 'source (if (eq value :missing) "inherited / production default"
                              (concat (org-glance-config-file:path
                                       (org-glance-graph:directory (org-glance-settings-draft-graph session)))
                                      "#/" (when (org-glance-settings-draft-tag session)
                                              (concat "tags/" (org-glance-settings-draft-tag session) "/")) field)))
              (cons 'state state))))
         (org-glance-config:workflow-fields (org-glance-settings-draft-tag session)))
      (append
       (cl-loop for item in (org-glance-settings--items) for n from 0
                collect (if (equal org-glance-settings--page "todoCycles")
                            (org-glance-settings--row
                             (number-to-string n) (cons 'order (number-to-string (1+ n)))
                             (cons 'active (string-join (append (gethash "active" item []) nil) " "))
                             (cons 'inactive (string-join (append (gethash "inactive" item []) nil) " ")))
                          (org-glance-settings--row
                           (number-to-string n) (cons 'order (number-to-string (1+ n)))
                           (cons 'text (car item))
                           (cons 'ending (car (rassoc (cdr item) org-glance-settings--endings))))))
       (when (eq (gethash org-glance-settings--page values :missing) :missing)
         (list (org-glance-settings--row "inherited" (cons 'order "")
                                       (cons 'active "Inherited cycles")
                                       (cons 'text "Inherited template · + creates an override"))))
       (list (org-glance-settings--row "new" (cons 'order "+") (cons 'active "Add cycle")
                                     (cons 'text "Add line")))))))

(defun org-glance-settings--spec ()
  "Return the current settings table schema."
  `((title . ,(concat "Settings · " (if (org-glance-settings-draft-tag org-glance-settings--session)
                                       (concat "tag:" (org-glance-settings-draft-tag org-glance-settings--session)) "system")
                      (when org-glance-settings--page (concat " → " org-glance-settings--page))))
    (subtitle . ,(or (org-glance-settings-draft-error org-glance-settings--session)
                     "Draft · C-c C-c saves · g reloads · R resets override · q goes back"))
    (columns . ,(mapcar (lambda (pair) `((key . ,(car pair)) (header . ,(cdr pair)) (type . "text")))
                        (cond ((null org-glance-settings--page)
                               '(("setting" . "Setting") ("value" . "Value") ("area" . "Area")
                                 ("applies" . "Applies to") ("source" . "Source") ("state" . "State")))
                              ((equal org-glance-settings--page "todoCycles")
                               '(("order" . "Order") ("active" . "Active") ("inactive" . "Done")))
                              (t '(("order" . "Order") ("text" . "Text") ("ending" . "Ending"))))))
    (rows . ,(org-glance-settings--rows))
    (actions . (((key . "RET") (command . "edit") (label . "Edit / open"))
                ((key . "+") (command . "add") (label . "Add"))
                ((key . "d") (command . "delete") (label . "Delete item"))
                ((key . "R") (command . "reset") (label . "Reset override"))
                ((key . "C-c C-c") (command . "save") (label . "Save"))
                ((key . "g") (command . "reload") (label . "Reload"))
                ((key . "q") (command . "back") (label . "Back"))))))

(defun org-glance-settings--fill ()
  "Refill the current table while retaining its row and cell."
  (let ((point (org-glance-view:point-context)))
    (setf (alist-get 'subtitle table-view--spec)
          (or (org-glance-settings-draft-error org-glance-settings--session)
              "Draft · C-c C-c saves · g reloads · R resets override · q goes back"))
    (table-view-set-rows (current-buffer) (org-glance-settings--rows))
    (apply #'org-glance-view:restore-point point)))

(defun org-glance-settings--kill-query ()
  "Require an explicit choice before discarding the current draft."
  (or (not (org-glance-settings--dirty org-glance-settings--session))
      (yes-or-no-p "Discard unsaved Settings draft? ")))

(defun org-glance-settings--mount (session page parent)
  "Mount SESSION's PAGE, retaining the suspended PARENT frame."
  (let* ((org-glance-settings--session session)
         (org-glance-settings--page page)
         (spec (org-glance-settings--spec)))
    (table-view-display (current-buffer) spec
                        '(("edit" . org-glance-settings--edit)
                          ("add" . org-glance-settings--add)
                          ("delete" . org-glance-settings--delete)
                          ("reset" . org-glance-settings--reset)
                          ("save" . org-glance-settings--save)
                          ("reload" . org-glance-settings--reload)
                          ("back" . org-glance-settings--back))))
  (setq org-glance-settings--session session org-glance-settings--page page
        org-glance-settings--parent parent
        default-directory (file-name-as-directory (org-glance-graph:directory
                                                    (org-glance-settings-draft-graph session))))
  (add-hook 'kill-buffer-query-functions #'org-glance-settings--kill-query nil t)
  (local-set-key (kbd "C-x C-s") (lambda () (interactive) (org-glance-settings--save nil nil)))
  (dolist (key '("<escape>" "DEL"))
    (local-set-key (kbd key) (lambda () (interactive) (org-glance-settings--back nil nil)))))

(defun org-glance-settings--edit (id _row)
  "Edit the setting or child cell at ID."
  (when id
    (cond
     ((null org-glance-settings--page)
      (if (member id '("todoCycles" "template"))
          (org-glance-settings--mount
           org-glance-settings--session id
           (list (org-glance-view:point-context) table-view--filter table-view--marks
                 table-view--narrowed (table-view-layout)))
        (let* ((values (org-glance-settings-draft-values org-glance-settings--session))
               (previous (gethash id values ""))
               (value (if (equal id "storage")
                          (org-glance-settings--choose "Storage: " '("local" "portable") previous)
                        (read-string "Title: " previous))))
          (puthash id value values)
          (org-glance-settings--fill))))
     ((equal id "inherited") (user-error "Use + to create an override"))
     ((equal id "new") (org-glance-settings--add nil nil))
     (t
      (let* ((items (org-glance-settings--items)) (index (string-to-number id))
             (item (nth index items)) (column (get-text-property (point) 'table-view-col)))
        (when item
          (cond
           ((equal column "order")
            (let* ((raw (read-string "Order: " (number-to-string (1+ index))))
                   (n (string-to-number raw)))
              (unless (and (string-match-p "\\`[0-9]+\\'" raw) (<= 1 n) (<= n (length items)))
                (user-error "Order must be between 1 and %d" (length items)))
              (setq items (append (cl-subseq items 0 index) (nthcdr (1+ index) items)))
              (setq items (append (cl-subseq items 0 (1- n)) (list item) (nthcdr (1- n) items)))))
           ((equal org-glance-settings--page "todoCycles")
            (let ((field (if (equal column "inactive") "inactive" "active")))
              (setq item (copy-hash-table item))
              (puthash field (vconcat (split-string
                                      (read-string (concat field ": ")
                                                   (string-join (append (gethash field item []) nil) " ")) nil t)) item)
              (setf (nth index items) item)))
           ((equal column "ending")
            (setf (nth index items)
                  (cons (car item) (cdr (assoc (org-glance-settings--choose
                                               "Ending: " (mapcar #'car org-glance-settings--endings)
                                               (car (rassoc (cdr item) org-glance-settings--endings)))
                                             org-glance-settings--endings)))))
           (t (setf (nth index items) (cons (org-glance-settings--read-line (car item)) (cdr item)))))
          (org-glance-settings--set-items items)
          (org-glance-settings--fill)))))))

(defun org-glance-settings--read-line (initial)
  "Read one template line starting at INITIAL."
  (let ((text (read-string "Text: " initial)))
    (when (string-match-p "[\r\n]" text) (user-error "Enter one template line"))
    text))

(defun org-glance-settings--add (_id _row)
  "Append a staged child item."
  (unless org-glance-settings--page (user-error "Open cycles or template first"))
  (let* ((items (org-glance-settings--items))
         (item (if (equal org-glance-settings--page "todoCycles")
                   (let ((cycle (make-hash-table :test 'equal)))
                     (puthash "active" (vconcat (split-string (read-string "Active keywords: ") nil t)) cycle)
                     (puthash "inactive" (vconcat (split-string (read-string "Done keywords: ") nil t)) cycle)
                     cycle)
                 (cons (org-glance-settings--read-line "")
                       (or (cl-some (lambda (line) (unless (equal (cdr line) "") (cdr line))) items) "\n")))))
    (when (and (equal org-glance-settings--page "template") items
               (equal (cdr (car (last items))) ""))
      (setq items (append (butlast items) (list (cons (car (car (last items))) (cdr item))))))
    (org-glance-settings--set-items (append items (list item)))
    (org-glance-settings--fill)
    (table-view--goto-id (number-to-string (length items)))
    (table-view--goto-cell (if (equal org-glance-settings--page "todoCycles") "active" "text"))))

(defun org-glance-settings--delete (id _row)
  "Delete child item ID from the draft."
  (unless org-glance-settings--page (user-error "Use R to reset a setting override"))
  (when (equal id "inherited") (user-error "Use + to create an override"))
  (when (and id (not (equal id "new")))
    (let ((items (org-glance-settings--items)) (n (string-to-number id)))
      (org-glance-settings--set-items (append (cl-subseq items 0 n) (nthcdr (1+ n) items)))
      (org-glance-settings--fill))))

(defun org-glance-settings--reset (id _row)
  "Remove the override at ID or the open child's setting."
  (when-let* ((field (or org-glance-settings--page id)))
    (remhash field (org-glance-settings-draft-values org-glance-settings--session))
    (org-glance-settings--fill)))

(defun org-glance-settings--save (_id _row)
  "Publish the complete draft and retain any failed-apply receipt."
  (let ((session org-glance-settings--session))
    (condition-case err
        (let ((saved (org-glance-config:write-workflow
                      (org-glance-settings-draft-graph session) (org-glance-settings-draft-tag session)
                      (org-glance-settings-draft-values session)
                      :expected (org-glance-settings-draft-revision session))))
          (setf (org-glance-settings-draft-revision session) (org-glance-config-file:revision saved))
          (org-glance-config:refresh (org-glance-settings-draft-graph session))
          (when-let* ((failure (org-glance-graph:-config-error (org-glance-settings-draft-graph session))))
            (user-error "Settings saved; live application failed: %s" failure))
          (setf (org-glance-settings-draft-base session)
                (org-glance-settings--copy (org-glance-settings-draft-values session))
                (org-glance-settings-draft-error session) nil)
          (org-glance-settings--fill)
          (message "Settings saved"))
      (error
       (setf (org-glance-settings-draft-error session) (error-message-string err))
       (org-glance-settings--fill)
       (signal (car err) (cdr err))))))

(defun org-glance-settings--reload (_id _row)
  "Reconcile a fresh observation, explicitly choosing overlapping edits."
  (let* ((old org-glance-settings--session)
         (fresh (org-glance-settings--observe (org-glance-settings-draft-graph old)
                                              (org-glance-settings-draft-tag old)))
         (values (org-glance-settings-draft-values fresh)))
    (dolist (field (org-glance-config:workflow-fields (org-glance-settings-draft-tag old)))
      (let* ((before (gethash field (org-glance-settings-draft-base old) :missing))
             (draft (gethash field (org-glance-settings-draft-values old) :missing))
             (current (gethash field values :missing)))
        (unless (org-glance-config-file--equal before draft)
          (when (or (org-glance-config-file--equal before current)
                    (org-glance-config-file--equal draft current)
                    (equal "draft" (org-glance-settings--choose
                                     (format "%s changed concurrently; keep: " field)
                                     '("current" "draft"))))
            (if (eq draft :missing) (remhash field values) (puthash field draft values))))))
    (org-glance-config:refresh (org-glance-settings-draft-graph old))
    (when-let* ((failure (org-glance-graph:-config-error (org-glance-settings-draft-graph old))))
      (user-error "Settings refresh failed; draft retained: %s" failure))
    (setq org-glance-settings--session fresh)
    (org-glance-settings--fill)))

(defun org-glance-settings--back (_id _row)
  "Restore the parent table or close after the draft-discard check."
  (if org-glance-settings--page
      (let ((frame org-glance-settings--parent))
        (org-glance-settings--mount org-glance-settings--session nil nil)
        (setq table-view--filter (nth 1 frame) table-view--marks (nth 2 frame)
              table-view--narrowed (nth 3 frame))
        (setf (alist-get 'columns table-view--spec) (plist-get (nth 4 frame) :columns))
        (table-view-set-sort (plist-get (nth 4 frame) :sort))
        (org-glance-settings--fill)
        (apply #'org-glance-view:restore-point (car frame)))
    (when (org-glance-settings--kill-query)
      (setf (org-glance-settings-draft-values org-glance-settings--session)
            (org-glance-settings--copy (org-glance-settings-draft-base org-glance-settings--session))
            (org-glance-settings-draft-error org-glance-settings--session) nil)
      (quit-window t))))

(defun org-glance-settings:visit (graph &optional tag)
  "Open GRAPH's JSON settings for TAG, nil for system, retaining existing drafts."
  (when tag
    (unless (org-glance-config--tag tag) (user-error "Invalid settings tag"))
    (setq tag (org-glance-config--tag-name tag)))
  (let ((name (format "*org-glance Settings %s %s*"
                      (substring (secure-hash 'sha256 (org-glance-graph:directory graph)) 0 12)
                      (if tag (concat "tag:" tag) "system"))))
    (if (buffer-live-p (get-buffer name))
        (progn
          (with-current-buffer name
            (unless (and (org-glance-settings-draft-p org-glance-settings--session)
                         (equal tag (org-glance-settings-draft-tag org-glance-settings--session))
                         (equal (org-glance-graph:directory graph)
                                (org-glance-graph:directory
                                 (org-glance-settings-draft-graph org-glance-settings--session))))
              (user-error "Existing buffer does not belong to this Settings scope")))
          (switch-to-buffer name))
      (let ((session (org-glance-settings--observe graph tag)))
        (switch-to-buffer (get-buffer-create name))
        (org-glance-settings--mount session nil nil)))
    (current-buffer)))

;;;###autoload
(defun org-glance-settings ()
  "Open the current tree's system Settings table."
  (interactive)
  (org-glance-settings:visit (org-glance-ensure-init)))

(provide 'org-glance-settings)
;;; org-glance-settings.el ends here
