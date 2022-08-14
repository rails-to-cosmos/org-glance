(require 'cl-macs)

(require 'org-glance-event)

(cl-defstruct (org-glance-log (:constructor org-glance-log--create)
                              (:copier nil))
  (events nil :type list :read-only nil)
  (hash-test #'equal :type function :read-only t)
  (id-determinator #'identity :type function :read-only t :documentation "Unique event ID determinator."))

(cl-defun org-glance-log:create (events id-determinator)
  (org-glance-log--create
   :id-determinator id-determinator
   :events events))

(cl-defun org-glance-log:flatten (log)
  "Return list of LOG events deduplicated."
  (cl-loop
     with seen = (make-hash-table :test (org-glance-log-hash-test log))
     for event in (org-glance-log-events log)
     for key = (funcall (org-glance-log-id-determinator log) event)
     unless (gethash key seen)
     collect (prog1 event (puthash key t seen))))

(cl-defun org-glance-log:contents (log)
  "Return LOG contents as a string."
  (->> log
       org-glance-log:flatten
       reverse
       (mapcar #'prin1-to-string)
       (s-join "\n")))

(cl-defmacro org-glance-log:push (log event)
  "Append ENTRIES to LOG."
  `(push ,event (org-glance-log-events ,log)))

(cl-defmacro org-glance-log:pop (log)
  "Pop from LOG."
  `(pop (org-glance-log-events ,log)))

(cl-defun org-glance-log:read (location id-determinator)
  (unless (and (f-exists-p location) (f-readable-p location))
    (user-error "Location %s does not exist." location))
  (with-temp-buffer
    (let ((result (org-glance-log:create nil id-determinator)))
      (while (not (eobp))
        (org-glance-log:push result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (forward-line))
      result)))

(cl-defun org-glance-log:write (log location)
  (let ((contents (org-glance-log:contents log)))
    (cond ((and (f-exists-p location) (f-empty-p location))
           (append-to-file contents nil location))
          ((f-exists-p location)
           (append-to-file (concat "\n" contents) nil location))
          (t (org-glance--with-temp-file location
               (insert contents))))))

(cl-defun org-glance-log:merge (lhs rhs)
  (cl-assert (eq (org-glance-log-id-determinator lhs) (org-glance-log-id-determinator rhs)))
  (org-glance-log:create
   (append (org-glance-log-events lhs)
           (org-glance-log-events rhs))
   (org-glance-log-id-determinator lhs)))

(cl-defun org-glance-log:last (log)
  (car (org-glance-log-events log)))

(cl-defun org-glance-log:filter (log func)
  (cl-loop
     for event in (org-glance-log:flatten log)
     when (funcall func event)
     collect event into events
     finally return (org-glance-log:create events (org-glance-log-id-determinator log))))

(cl-defun org-glance-log:empty-p (log)
  (null (org-glance-log-events log)))

(provide 'org-glance-log)
