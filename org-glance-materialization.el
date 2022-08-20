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

(org-glance-class org-glance-marker nil
    ((beg
      :type number
      :initarg :beg
      :reader org-glance-marker:beg nil
      :documentation "Beginning of headline.")
     (end
      :type number
      :initarg :end
      :reader org-glance-marker:end nil
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
     (changed-p
      :type boolean
      :initarg :changed-p
      :reader org-glance-marker:changed-p
      :documentation "Has headline changed?")
     (persisted-p
      :type boolean
      :initarg :persisted-p
      :reader org-glance-marker:persisted-p
      :documentation "Whether headline has origin or not.")
     (committed-p
      :type boolean
      :initarg :committed-p
      :reader org-glance-marker:committed-p
      :documentation "Whether changes have been committed.")
     (outdated-p
      :type boolean
      :initarg :outdated-p
      :reader org-glance-marker:outdated-p
      :documentation "If there are changes that are not reflected in current materialization."))
  "Metadata of materializations.")

(cl-defun org-glance-marker:at-point ()
  (get-text-property (point) :marker))

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
          (puthash filename (org-glance-view:materialization view filename) org-glance-materializations)))))

(cl-defun org-glance-materialization:prepare-markers (materialization)
  (org-glance-map (headline)
    (let ((marker (org-glance-marker
                   :hash (org-glance-headline:hash headline)
                   :beg (point-min)
                   :end (point-max)
                   :buffer (current-buffer)
                   :overlay (make-overlay (point-min) (point-min))
                   :changed-p nil
                   :committed-p nil
                   :persisted-p (not (null headline))
                   :outdated-p nil)))
      (add-text-properties (point-min) (point-max) (list :marker marker))
      ;; (save-buffer) ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
      (puthash marker (point-min) (org-glance-> materialization :marker->point)))))

(cl-defun org-glance-materialization:update (materialization)
  (puthash (org-glance-marker:at-point) (point) (org-glance-> materialization :marker->point)))

(cl-defmacro org-glance-materialization:do-markers (materialization &rest forms)
  (declare (indent 1))
  `(cl-loop
      for marker being the hash-keys of (org-glance-> ,materialization :marker->point)
      using (hash-values point)
      when (and marker
                (org-glance-marker:buffer marker)
                (buffer-live-p (org-glance-marker:buffer marker)))
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
      when (and marker
                (org-glance-marker:buffer marker)
                (eq (current-buffer) (org-glance-marker:buffer marker)))
      do (with-current-buffer (org-glance-marker:buffer marker)
           (save-excursion
             (save-restriction
               (widen)
               (let ((,(car spec) marker))
                 ,@body))))))

(cl-defun org-glance-materialization:commit (materialization)
  (let ((store (org-glance-> materialization :view :store)))
    (org-glance-materialization:do-changes (marker materialization)
      (let ((headline (org-glance-headline-from-region
                       (org-glance-marker:beg marker)
                       (org-glance-marker:end marker))))
        (org-glance-store:put store headline)))
    (org-glance-store:flush store)))

(cl-defun org-glance-marker:print (marker)
  (prin1 (a-list
          :beg (org-glance-> marker :beg)
          :end (org-glance-> marker :end)
          :hash (org-glance-> marker :hash)
          :overlay (org-glance-> marker :overlay)
          :changed-p (org-glance-> marker :changed-p)
          :persisted-p (org-glance-> marker :persisted-p)
          :committed-p (org-glance-> marker :committed-p)
          :offset (org-glance-> marker :offset))))

(provide 'org-glance-materialization)
