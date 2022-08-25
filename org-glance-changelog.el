;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-event)

(org-glance-class org-glance-changelog nil
    ((events :type list
             :initarg :events
             :initform nil)
     (test :type function
                :initarg :test
                :initform #'equal)
     (key :type function
          :initarg :key
          :initform #'identity)))

(cl-defun org-glance-changelog:flatten (log)
  "Return list of LOG events deduplicated."
  (cl-loop
     with seen = (make-hash-table :test (org-glance-> log :test))
     for event in (org-glance-> log :events)
     for key = (funcall (org-glance-> log :key) event)
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
  `(push ,event (org-glance-> ,log :events)))

(cl-defmacro org-glance-changelog:pop (log)
  "Pop from LOG."
  `(pop (org-glance-> ,log :events)))

(cl-defun org-glance-changelog:read (location &key test)
  (declare (indent 1))
  (cond ((and (f-exists-p location) (f-readable-p location))
         (with-temp-buffer
           (insert-file-contents location)
           (let ((result (org-glance-changelog :key test)))
             (while (not (eobp))
               (org-glance-changelog:push result
                 (read (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               (forward-line))
             result)))
        (t (org-glance-changelog :key test))))

(cl-defun org-glance-changelog:write (log location)
  (let ((contents (org-glance-changelog:contents log)))
    (when (not (string-empty-p contents))
      (org-glance--with-temp-file location
        (insert contents)))))

(cl-defun org-glance-changelog:merge (lhs rhs)
  (cl-assert (eq (org-glance-> lhs :key) (org-glance-> rhs :key)))
  (org-glance-changelog
   :events (append (org-glance-> lhs :events)
                   (org-glance-> rhs :events))
   :key (org-glance-> lhs :key)))

(cl-defun org-glance-changelog:last (log)
  (car (org-glance-> log :events)))

(cl-defun org-glance-changelog:filter (changelog func)
  (cl-loop
     for event in (org-glance-changelog:flatten changelog)
     when (funcall func event)
     collect event into events
     finally return (org-glance-changelog :events events
                                          :key (org-glance-> changelog :key))))

(cl-defun org-glance-changelog:length (log)
  (length (org-glance-> log :events)))

(cl-defun org-glance-changelog:empty-p (log)
  (null (org-glance-> log :events)))

(provide 'org-glance-changelog)
