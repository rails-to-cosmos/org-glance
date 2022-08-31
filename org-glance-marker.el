;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)
(require 'org-glance-headline)

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
      :documentation "Materialised buffer.")
     (hash
      :type string
      :initarg :hash
      :documentation "Hash of headline origin.")
     (offset
      :initform 0
      :initarg :offset
      :type number
      :documentation "Actual offset of headline.")
     (overlay
      :type overlay
      :initarg :overlay
      :documentation "Current overlay for status reporting.")
     (state
      :type org-glance-marker-state
      :initarg :state))
  "Metadata of mews.")

(cl-defun org-glance-marker:live-p (marker)
  (and marker
       (org-glance-> marker :buffer)
       (buffer-live-p (org-glance-> marker :buffer))))

(cl-defun org-glance-marker:at-point ()
  "Return instance of `org-glance-marker' from text at point."
  ;; avoid strange behaviour on (point) == (point-max)
  (or (get-text-property (point) :marker)
      (save-excursion
        (goto-char (point-max))
        (org-back-to-heading-or-point-min)
        (get-text-property (point) :marker))))

(cl-defun org-glance-marker:get-actual-state (marker headline)
  (let ((hash-old (org-glance-> marker :hash))
        (hash-new (org-glance-> headline :hash)))
    (org-glance-marker-state
     :changed (not (string= hash-old hash-new)))))

(cl-defun org-glance-marker:headline (marker)
  "Create instance of `org-glance-headline' from MARKER."
  (org-glance-headline-from-region
   (org-glance-> marker :beg)
   (org-glance-> marker :end)))

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
                      :outdated (org-glance-> marker :state :outdated))
              :offset (org-glance-> marker :offset))))
    (json-pretty-print-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmacro org-glance-marker:with-current-buffer (marker &rest forms)
  (declare (indent 1))
  `(with-current-buffer (org-glance-> ,marker :buffer)
     (save-excursion
       (save-restriction
         (widen)
         ,@forms))))

(cl-defun org-glance-marker:redisplay (marker)
  "Refresh MARKER overlay."
  (let ((beg (org-glance-> marker :beg))
        (marked (slot-boundp marker :overlay))
        (changed (org-glance-> marker :state :changed))
        (committed (org-glance-> marker :state :committed))
        (corrupted (org-glance-> marker :state :corrupted))
        (outdated (org-glance-> marker :state :outdated)))
    (cond ((and changed (not marked))
           (let ((overlay (make-overlay beg (1+ beg))))
             (setf (org-glance-> marker :overlay) overlay)
             (overlay-put overlay 'face '(:foreground "#ffcc00"))))
          ((and changed committed marked)
           (delete-overlay (org-glance-> marker :overlay))
           (slot-makeunbound marker :overlay)
           (let ((overlay (make-overlay beg (1+ beg))))
             (setf (org-glance-> marker :overlay) overlay
                   (org-glance-> marker :state :committed) nil)
             (overlay-put overlay 'face '(:foreground "#ffcc00"))))
          ((and (not changed) (not committed) (not corrupted) marked)
           (progn
             (delete-overlay (org-glance-> marker :overlay))
             (slot-makeunbound marker :overlay)))
          ((and (not changed) marked committed)
           (progn
             (delete-overlay (org-glance-> marker :overlay))
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-> marker :overlay) overlay)
               (overlay-put overlay 'face '(:foreground "#27ae60")))))
          (outdated
           (progn
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-> marker :overlay) overlay)
               (overlay-put overlay 'face '(:foreground "#749AF7")))))
          ((and corrupted (not marked))
           (let ((overlay (make-overlay beg (1+ beg))))
             (setf (org-glance-> marker :overlay) overlay)
             (overlay-put overlay 'face '(:foreground "#e74c3c")))))))

(provide 'org-glance-marker)
