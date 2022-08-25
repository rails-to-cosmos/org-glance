;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-event)

(cl-defstruct (org-glance-changelog (:constructor org-glance-changelog--create)
                                    (:copier nil))
  (events nil :type list :read-only nil)
  (hash-test #'equal :type function :read-only t)
  ;; refactor to KEY
  (test #'identity :type function :read-only t :documentation "Unique event ID determinator."))

(cl-defun org-glance-changelog:create (events test)
  (org-glance-changelog--create :test test :events events))

(cl-defun org-glance-changelog:flatten (log)
  "Return list of LOG events deduplicated."
  (cl-loop
     with seen = (make-hash-table :test (org-glance-changelog-hash-test log))
     for event in (org-glance-changelog-events log)
     for key = (funcall (org-glance-changelog-test log) event)
     unless (gethash key seen)
     collect (prog1 event (puthash key t seen))))

(cl-defun org-glance-changelog:contents (log)
  "Return LOG contents as a string."
  (->> log
       org-glance-changelog:flatten
       reverse
       (mapcar #'prin1-to-string)
       (s-join "\n")
       s-trim))

(cl-defmacro org-glance-changelog:push (log event)
  "Append ENTRIES to LOG."
  (declare (indent 1))
  `(push ,event (org-glance-changelog-events ,log)))

(cl-defmacro org-glance-changelog:pop (log)
  "Pop from LOG."
  `(pop (org-glance-changelog-events ,log)))

(cl-defun org-glance-changelog:read (location &key test)
  (declare (indent 1))
  (cond ((and (f-exists-p location) (f-readable-p location))
         (with-temp-buffer
           (insert-file-contents location)
           (let ((result (org-glance-changelog:create nil test)))
             (while (not (eobp))
               (org-glance-changelog:push result
                                          (read (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               (forward-line))
             result)))
        (t (org-glance-changelog:create nil test))))

(cl-defun org-glance-changelog:write (log location)
  (let ((contents (org-glance-changelog:contents log)))
    (when (not (string-empty-p contents))
      (cond ((and (f-exists-p location) (f-empty-p location))
             (append-to-file contents nil location))
            ((f-exists-p location)
             (append-to-file (concat "\n" contents) nil location))
            (t (org-glance--with-temp-file location
                 (insert contents)))))))

(cl-defun org-glance-changelog:merge (lhs rhs)
  (cl-assert (eq (org-glance-changelog-test lhs) (org-glance-changelog-test rhs)))
  (org-glance-changelog:create
   (append (org-glance-changelog-events lhs)
           (org-glance-changelog-events rhs))
   (org-glance-changelog-test lhs)))

(cl-defun org-glance-changelog:last (log)
  (car (org-glance-changelog-events log)))

(cl-defun org-glance-changelog:filter (log func)
  (cl-loop
     for event in (org-glance-changelog:flatten log)
     when (funcall func event)
     collect event into events
     finally return (org-glance-changelog:create events (org-glance-changelog-test log))))

(cl-defun org-glance-changelog:length (log)
  (length (org-glance-changelog-events log)))

(cl-defun org-glance-changelog:empty-p (log)
  (null (org-glance-changelog-events log)))

(provide 'org-glance-changelog)
