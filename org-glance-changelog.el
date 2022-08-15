;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-event)

(cl-defstruct (org-glance-changelog (:constructor org-glance-changelog--create)
                                    (:copier nil))
  (events nil :type list :read-only nil)
  (hash-test #'equal :type function :read-only t)
  (id-determinator #'identity :type function :read-only t :documentation "Unique event ID determinator."))

(cl-defun org-glance-changelog:create (events id-determinator)
  (org-glance-changelog--create
   :id-determinator id-determinator
   :events events))

(cl-defun org-glance-changelog:flatten (log)
  "Return list of LOG events deduplicated."
  (cl-loop
     with seen = (make-hash-table :test (org-glance-changelog-hash-test log))
     for event in (org-glance-changelog-events log)
     for key = (funcall (org-glance-changelog-id-determinator log) event)
     unless (gethash key seen)
     collect (prog1 event (puthash key t seen))))

(cl-defun org-glance-changelog:contents (log)
  "Return LOG contents as a string."
  (->> log
       org-glance-changelog:flatten
       reverse
       (mapcar #'prin1-to-string)
       (s-join "\n")))

(cl-defmacro org-glance-changelog:push (log event)
  "Append ENTRIES to LOG."
  `(push ,event (org-glance-changelog-events ,log)))

(cl-defmacro org-glance-changelog:pop (log)
  "Pop from LOG."
  `(pop (org-glance-changelog-events ,log)))

(cl-defun org-glance-changelog:read (location id-determinator)
  (cond ((and (f-exists-p location) (f-readable-p location))
         (with-temp-buffer
           (let ((result (org-glance-changelog:create nil id-determinator)))
             (while (not (eobp))
               (org-glance-changelog:push result (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (forward-line))
             result)))
        (t (org-glance-changelog:create nil id-determinator))))

(cl-defun org-glance-changelog:write (log location)
  (let ((contents (org-glance-changelog:contents log)))
    (cond ((and (f-exists-p location) (f-empty-p location))
           (append-to-file contents nil location))
          ((f-exists-p location)
           (append-to-file (concat "\n" contents) nil location))
          (t (org-glance--with-temp-file location
               (insert contents))))))

(cl-defun org-glance-changelog:merge (lhs rhs)
  (cl-assert (eq (org-glance-changelog-id-determinator lhs) (org-glance-changelog-id-determinator rhs)))
  (org-glance-changelog:create
   (append (org-glance-changelog-events lhs)
           (org-glance-changelog-events rhs))
   (org-glance-changelog-id-determinator lhs)))

(cl-defun org-glance-changelog:last (log)
  (car (org-glance-changelog-events log)))

(cl-defun org-glance-changelog:filter (log func)
  (cl-loop
     for event in (org-glance-changelog:flatten log)
     when (funcall func event)
     collect event into events
     finally return (org-glance-changelog:create events (org-glance-changelog-id-determinator log))))

(cl-defun org-glance-changelog:length (log)
  (length (org-glance-changelog-events log)))

(cl-defun org-glance-changelog:empty-p (log)
  (null (org-glance-changelog-events log)))

(provide 'org-glance-changelog)
