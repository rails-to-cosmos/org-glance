;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)
(require 'org-glance-headline)

(org-glance-class org-glance-marker-state nil
    ((changed
      :type boolean
      :initarg :changed
      :initform nil)
     (persisted
      :type boolean
      :initarg :persisted
      :initform t)
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
      :reader org-glance-marker:beg
      :documentation "Beginning of headline.")
     (end
      :type number
      :initarg :end
      :reader org-glance-marker:end
      :documentation "End of headline.")
     (buffer
      :type buffer
      :initarg :buffer
      :reader org-glance-marker:buffer
      :documentation "Materialized buffer.")
     (hash
      :type string
      :initarg :hash
      :reader org-glance-marker:hash
      :documentation "Hash of headline origin.")
     (offset
      :initform 0
      :initarg :offset
      :type number
      :reader org-glance-marker:offset
      :documentation "Actual offset of headline.")
     (overlay
      :type overlay
      :initarg :overlay
      :reader org-glance-marker:overlay
      :documentation "Current overlay for status reporting.")
     (state
      :type org-glance-marker-state
      :initarg :state
      :reader org-glance-marker:state))
  "Metadata of materializations.")

(cl-defun org-glance-marker:live-p (marker)
  (and marker
       (org-glance-marker:buffer marker)
       (buffer-live-p (org-glance-marker:buffer marker))))

(cl-defun org-glance-marker:at-point ()
  "Return instance of `org-glance-marker' from text at point."
  (get-text-property (point) :marker))

(cl-defun org-glance-marker:get-actual-state (marker headline)
  (let ((hash-old (org-glance-marker:hash marker))
        (hash-new (org-glance-headline:hash headline)))
    (org-glance-marker-state
     :changed (not (string= hash-old hash-new))
     :persisted (org-glance-> marker :state :persisted))))

(cl-defmacro org-glance-map-markers (var &rest forms)
  (declare (indent 1))
  `(org-glance-map-headlines (headline)
     (let ((,(car var) (org-glance-marker
                        :hash (org-glance-headline:hash headline)
                        :beg (point-min) ;; beginning of headline in narrowed buffer
                        :end (point-max) ;; end of headline in narrowed buffer
                        :buffer (current-buffer)
                        :state (org-glance-marker-state))))
       ,@forms)))

(cl-defun org-glance-marker:headline (marker)
  "Create instance of `org-glance-headline' from MARKER."
  (org-glance-headline-from-region
   (org-glance-marker:beg marker)
   (org-glance-marker:end marker)))

(cl-defun org-glance-marker:prin1-to-string (marker)
  (prin1-to-string
   (a-list
    :beg (org-glance-> marker :beg)
    :end (org-glance-> marker :end)
    :hash (org-glance-> marker :hash)
    :overlay (when (slot-boundp marker :overlay)
               (org-glance-> marker :overlay))
    :changed (org-glance-> marker :state :changed)
    :persisted (org-glance-> marker :state :persisted)
    :committed (org-glance-> marker :state :committed)
    :offset (org-glance-> marker :offset))
   t))

(cl-defun org-glance-state:prin1-to-string (state)
  (with-temp-buffer
    (insert (json-encode-alist
             (a-list
              :changed (org-glance-> state :changed)
              :persisted (org-glance-> state :persisted)
              :committed (org-glance-> state :committed)
              :outdated (org-glance-> state :outdated))))
    (json-pretty-print-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'org-glance-marker)
