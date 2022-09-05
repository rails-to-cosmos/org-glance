;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)
(require 'org-glance-headline)

;; TODO use bool-vector instead
(org-glance-class org-glance-marker-state nil
    ((changed
      :type boolean
      :initarg :changed
      :initform nil)
     (corrupted
      :type boolean
      :initarg :corrupted
      :initform nil)
     (committed
      :type boolean
      :initarg :committed
      :initform nil)
     (outdated
      :type boolean
      :initarg :outdated
      :initform nil)))

(org-glance-class org-glance-marker nil
    ((beg
      :type number
      :initarg :beg
      :documentation "Beginning of headline.")
     (end
      :type number
      :initarg :end
      :documentation "End of headline.")
     (buffer
      :type buffer
      :initarg :buffer
      :documentation "Materialized buffer.")
     (hash
      :type string
      :initarg :hash
      :documentation "Hash of headline origin.")
     (overlay
      :type overlay
      :initarg :overlay
      :documentation "Current overlay for status reporting.")
     (state
      :type org-glance-marker-state
      :initarg :state))
  "Metadata of mews.")

(cl-defun org-glance-marker:prin1-to-string (marker)
  (with-temp-buffer
    (insert (json-encode-alist
             (a-list
              :beg (org-glance-> marker :beg)
              :end (org-glance-> marker :end)
              :hash (org-glance-> marker :hash)
              :overlay (when (slot-boundp marker :overlay)
                         (prin1-to-string (org-glance-> marker :overlay)))
              :state (a-list
                      :changed (org-glance-> marker :state :changed)
                      :corrupted (org-glance-> marker :state :corrupted)
                      :committed (org-glance-> marker :state :committed)
                      :outdated (org-glance-> marker :state :outdated)))))
    (json-pretty-print-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmacro org-glance-marker:with-current-buffer (marker &rest forms)
  (declare (indent 1))
  `(with-current-buffer (org-glance-> ,marker :buffer)
     (save-excursion
       (save-restriction
         (widen)
         ,@forms))))

(provide 'org-glance-marker)
