;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-event)
(require 'org-glance-headline)

(org-glance-class org-glance-changelog nil
    ((events :type list
             :initarg :events
             :initform nil)
     (test :type function
           :initarg :test
           :initform #'equal)))

(cl-defun org-glance-changelog:flatten (changelog)
  "Return list of LOG events deduplicated."
  (org-glance-> changelog :events))

(cl-defun org-glance-changelog:contents (changelog)
  "Return CHANGELOG contents as a string."
  (thread-last changelog
    org-glance-changelog:flatten
    reverse
    (mapcar #'prin1-to-string)
    (s-join "\n")
    s-trim))

(cl-defmacro org-glance-changelog:push (changelog event)
  "Append ENTRIES to LOG."
  (declare (indent 1))
  `(push ,event (org-glance-> ,changelog :events)))

(cl-defun org-glance-changelog:read (location)
  (declare (indent 1))
  (cond ((and (f-exists-p location) (f-readable-p location))
         (with-temp-buffer
           (insert-file-contents location)
           (let ((result (org-glance-changelog)))
             (while (not (eobp))
               (org-glance-changelog:push result
                 (read (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               (forward-line))
             result)))
        (t (org-glance-changelog))))

(cl-defun org-glance-changelog:write (changelog location)
  (let ((contents (org-glance-changelog:contents changelog)))
    (when (not (string-empty-p contents))
      (org-glance--with-temp-file location
        (insert contents)))))

(cl-defun org-glance-changelog:merge (lhs rhs)
  (cl-assert (eq (org-glance-> lhs :test) (org-glance-> rhs :test)))
  (org-glance-changelog
   :events (append (org-glance-> lhs :events) (org-glance-> rhs :events))
   :test (org-glance-> lhs :test)))

(cl-defun org-glance-changelog:last (changelog)
  (car (org-glance-> changelog :events)))

(cl-defun org-glance-changelog:filter (changelog func)
  (cl-loop
     for event in (org-glance-changelog:flatten changelog)
     when (funcall func event)
     collect event into events
     finally return (org-glance-changelog
                     :events events
                     :test (org-glance-> changelog :test))))

(cl-defun org-glance-changelog:length (changelog)
  (length (org-glance-> changelog :events)))

(provide 'org-glance-changelog)
