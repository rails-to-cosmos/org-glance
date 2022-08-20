;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)
(require 'org-glance-headline)

(defalias 'org-glance-buffer-store 'org-glance-materialization:get-buffer-store)
(defalias 'org-glance-buffer-materialization 'org-glance-materialization:get-buffer-materialization)

(defvar org-glance-materializations (make-hash-table :test #'equal))

(org-glance-class org-glance-materialization nil
    ((view
      :type org-glance-view
      :initarg :view
      :reader org-glance-materialization:view
      :documentation "Backlink to source of materialization.")
     (location
      :type org-glance-file
      :initarg :location
      :reader org-glance-materialization:location
      :documentation "Location where materialization persists.")
     (marker->point
      :type hash-table
      :initarg :marker->point
      :initform (make-hash-table)
      :reader org-glance-materialization:marker->point
      :documentation "Maps markers to points.")
     (changes
      :type hash-table
      :initarg :changes
      :initform (make-hash-table)
      :reader org-glance-materialization:changes
      :documentation "Set of changed markers.")))

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

(cl-defun org-glance-marker:at-point ()
  (get-text-property (point) :marker))

(cl-defun org-glance-marker:get-actual-state (marker headline)
  (let ((hash-old (org-glance-marker:hash marker))
        (hash-new (org-glance-headline:hash headline)))
    (org-glance-marker-state
     :changed (not (string= hash-old hash-new))
     :persisted (org-glance-> marker :state :persisted))))

(cl-defun org-glance-marker-live-p (marker)
  (and marker
       (org-glance-marker:buffer marker)
       (buffer-live-p (org-glance-marker:buffer marker))))

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

(cl-defun org-glance-materialization:header (materialization)
  "Generate header for MATERIALIZATION."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                (format "#+STORE: %s" (org-glance-> materialization :view :store :location))
                (format "#+PREDICATE: %s" (org-glance-> materialization :view :predicate))
                ""
                "")))

(cl-defun org-glance-materialization:get-property (property &optional (mapper #'identity))
  (funcall mapper (save-excursion
                    (goto-char (point-min))
                    (search-forward (format "#+%s: " property))
                    (buffer-substring-no-properties (point) (line-end-position)))))

(cl-defun org-glance-materialization:get-buffer-store ()
  "Get `org-glance-store' instance associated with current buffer."
  (let ((location (org-glance-materialization:get-property "STORE")))
    (org-glance-store:read location)))

(cl-defun org-glance-materialization:get-buffer-view ()
  "Get `org-glance-view' instance associated with current buffer."
  (let ((store (org-glance-materialization:get-buffer-store))
        (predicate (org-glance-materialization:get-property "PREDICATE" #'intern)))
    (or (gethash (cons predicate store) org-glance-views)
        (puthash (cons predicate store) (org-glance-store:view store predicate) org-glance-views))))

(cl-defun org-glance-materialization:get-buffer-materialization ()
  "Get `org-glance-materialization' instance associated with current buffer."
  (let ((filename (file-truename (buffer-file-name))))
    (or (gethash filename org-glance-materializations)
        (let ((view (org-glance-materialization:get-buffer-view)))
          (puthash filename (org-glance-view:materialize view filename) org-glance-materializations)))))

(cl-defun org-glance-materialization:update (materialization)
  (puthash (org-glance-marker:at-point) (point) (org-glance-> materialization :marker->point)))

(cl-defmacro org-glance-materialization:do-markers (materialization &rest forms)
  (declare (indent 1))
  `(cl-loop
      for marker being the hash-keys of (org-glance-> ,materialization :marker->point)
      using (hash-values point)
      when (org-glance-marker-live-p marker)
      do (with-current-buffer (org-glance-marker:buffer marker)
           (save-excursion
             (save-restriction
               (widen)
               (goto-char point)
               (when (string= (org-glance-marker:hash (org-glance-marker:at-point))
                              (org-glance-marker:hash marker))
                 (org-glance--with-headline-at-point
                   ,@forms)))))))

(cl-defmacro org-glance-materialization:do-changes (spec &rest body)
  "Loop over changed markers in current buffer binding each marker to VAL and executing BODY.

\(fn (VAR MATERIALIZATION) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec))
    (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  `(cl-loop
      with materialization = ,(cadr spec)
      for marker being the hash-keys of (org-glance-> materialization :changes)
      when (org-glance-marker-live-p marker)
      do (with-current-buffer (org-glance-marker:buffer marker)
           (save-excursion
             (save-restriction
               (widen)
               (let ((,(car spec) marker))
                 ,@body))))))

(cl-defun org-glance-headline:from-marker (marker)
  (org-glance-headline-from-region
   (org-glance-marker:beg marker)
   (org-glance-marker:end marker)))

(cl-defun org-glance-materialization:commit (materialization)
  (let ((store (org-glance-> materialization :view :store)))
    (org-glance-materialization:do-changes (marker materialization)
      (let ((headline (org-glance-headline:from-marker marker)))
        (org-glance-store:put store headline)))
    (org-glance-store:flush store)))

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

(provide 'org-glance-materialization)
