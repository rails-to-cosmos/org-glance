;;; org-glance-view.el --- View of the world  -*- lexical-binding: t; -*-
;;; Commentary:
;;

(require 'eieio)
(require 'f)
(require 'org-macs)

(require 'org-glance-log)
(require 'org-glance-types)
(require 'org-glance-offset)
(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-dimension)
(require 'org-glance-vector)
(require 'org-glance-marker)

;;; Code:

(defconst org-glance-view-header-extension ".el")
(defconst org-glance-view-marker-extension ".m")

(declare-function f-mkdir-full-path 'f)

(org-glance-class ViewHeader ()
    ;; available TODO states
    ;; capture template
    ((type :type Partition :initarg :type
           :documentation "Type declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (offset :type Offset :initarg :offset)
     (size :type Number :initarg :size)))

(org-glance-class View (ViewHeader)
    ((location :type OptionalFile :initarg :location :documentation "Location where metadata is stored.")
     (markers :type Vector :initarg :markers :initform (org-glance-vector:create) :documentation "Dynamic array with headlines.")
     (hash->midx :type HashTable :initarg :hash->midx :initform (make-hash-table :test #'equal) :documentation "Hash to idx.")))

(org-glance-declare org-glance-view:create :: Partition -> OptionalFile -> Offset -> View)
(defun org-glance-view:create (type location offset)
  "Create `org-glance-view' instance from WORLD by TYPE and store it in LOCATION with initial OFFSET."
  (let ((view (org-glance-view :type type :location location :offset offset)))
    (unless (f-exists? location)
      (f-mkdir-full-path (f-parent location))
      (org-glance:with-temp-org-file location
        (insert (s-join "\n"
                        (list "#  -*- mode: org; mode: org-glance-material -*-"
                              ""
                              "#+STARTUP: overview"
                              ""
                              "")))))
    (org-glance-view:save-header view)
    view))

(cl-defmacro org-glance-view:if-safe-marker (view midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (org-glance-vector:size (org-glance? ,view :markers)))
       ,then
     ,@else))

(defun org-glance-view:get-marker-headline (view midx)
  (org-glance-view:if-safe-marker view midx
      (save-excursion
        (goto-char (org-glance-view:get-marker-position view midx))
        (org-glance-headline-at-point))))

(defun org-glance-view:get-marker-index (view hash)
  (gethash hash (org-glance? view :hash->midx)))

(defun org-glance-view:get-marker-position (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance? marker :position))
    (point-max)))

(defun org-glance-view:set-marker-position (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance! marker :position := val))))

(defun org-glance-view:set-marker-remove (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance! marker :position := val))))

(defun org-glance-view:set-marker-hash (view midx val)
  (org-glance-view:if-safe-marker view midx
      (progn
        (remhash (org-glance-view:get-marker-hash view midx) (org-glance? view :hash->midx))
        (puthash val midx (org-glance? view :hash->midx))
        (org-glance! view :markers [midx] :hash := val))))

(defun org-glance-view:get-marker-hash (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance? marker :hash))))

(defun org-glance-view:marker-changed? (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance? marker :changed?))))

(defun org-glance-view:set-marker-changed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance? view :markers) midx)))
        (org-glance! marker :changed? := val))))

(cl-defmacro org-glance-view:with-current-buffer (view &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (org-glance? ,view :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

(org-glance-declare org-glance-view:add-headline :: View -> Headline -> t)
(defun org-glance-view:add-headline (view headline)
  (let* ((markers (org-glance? view :markers))
         (hash (org-glance? headline :hash))
         (marker (org-glance-marker :hash hash :position (point-max))))
    (org-glance-vector:push-back! markers marker)
    (goto-char (point-max))
    (org-glance-headline:insert headline)
    (puthash hash (- (org-glance-vector:size markers) 1) (org-glance? view :hash->midx))))

(org-glance-declare org-glance-view:remove-headline :: View -> Hash -> t)
(defun org-glance-view:remove-headline (view hash)
  (org-glance-view:with-current-buffer view
    (let* ((midx (org-glance-view:get-marker-index view hash))
           (mpos (org-glance-view:get-marker-position view midx))
           (markers (org-glance? view :markers)))
      (goto-char mpos)
      (org-glance-headline:with-headline-at-point
        (let ((inhibit-modification-hooks t))
          (delete-region (point-min) (point-max))
          (org-glance-vector:remove-at! markers midx)
          (remhash hash (org-glance? view :hash->midx)))))))

(org-glance-declare org-glance-view:set-markers! :: View -> Vector -> t)
(defun org-glance-view:set-markers! (view markers)
  (org-glance-log :markers "Set markers: %s" markers)
  (cl-loop
     with hash->midx = (make-hash-table :test #'equal)
     for midx below (org-glance-vector:size markers)
     for marker = (org-glance-vector:get markers midx)
     do (puthash (org-glance? marker :hash) midx hash->midx)
     finally do (setf (org-glance? view :markers) markers
                      (org-glance? view :hash->midx) hash->midx)))

(org-glance-declare org-glance-view:mark-current-buffer :: View -> t)
(defun org-glance-view:mark-current-buffer (view)
  "Create effective representation of VIEW headline positions."
  (pcase (org-glance-view:load-markers view)
    ('()
      (org-glance-log :markers "Markers not loaded")
      (let ((markers (let ((markers (org-glance-vector:create)))
                       (org-glance-log :cache "[org-glance-view:mark] cache miss: make markers")
                       (org-glance-headline:map (headline)
                         (let ((marker (org-glance-marker :hash (org-glance? headline :hash)
                                                          :position (point-min))))
                           (org-glance-vector:push-back! markers marker)))
                       markers)))
        (org-glance-view:set-markers! view markers)
        (org-glance-log :markers "Save markers: %s" (org-glance? view :markers))
        (org-glance-view:save-markers view)))
    (markers
     (org-glance-log :markers "Markers loaded: %s" markers)
     (org-glance-view:set-markers! view markers))))

(org-glance-declare org-glance-view:save-markers :: View -> org-glance-vector)
(defun org-glance-view:save-markers (view)
  (let ((markers (org-glance? view :markers))
        (buffer-hash (buffer-hash)))
    (org-glance:with-temp-file (org-glance-view:locate-markers view)
      (insert (format "%s\n" buffer-hash))
      (cl-loop for midx below (org-glance-vector:size markers)
         for marker = (org-glance-vector:get markers midx)
         do
           (org-glance-log :markers "Save marker %d" midx)
           (insert (format "%s %d\n" (org-glance? marker :hash) (org-glance? marker :position)))))
    markers))

(org-glance-declare org-glance-view:load-markers :: View -> t)
(defun org-glance-view:load-markers (view)
  (let ((location (org-glance-view:locate-markers view)))
    (when (f-exists? location)
      (org-glance-view:with-current-buffer view
        (let ((view-hash (buffer-hash)))
          (with-temp-buffer
            (insert-file-contents location)
            (goto-char (point-min))

            (thunk-let ((mark-hash (buffer-substring-no-properties (point) (line-end-position))))
              (cond
                ((string= mark-hash view-hash) (cl-loop with markers = (org-glance-vector:create)
                                                  while (and (= 0 (forward-line 1)) (not (eobp)))
                                                  for marker = (cl-destructuring-bind (hash position)
                                                                   (s-split " " (buffer-substring-no-properties (point) (line-end-position)))
                                                                 (org-glance-marker :hash hash :position (string-to-number position)))
                                                  do (org-glance-vector:push-back! markers marker)
                                                  finally return markers))
                (t (org-glance-log :cache "Mark cache validation failed: \"%s\" vs \"%s\""
                     (buffer-substring-no-properties (point-min) (line-end-position))
                     view-hash)
                   nil)))))))))

(defun org-glance-view:get-offset (view)
  (let ((buffer-offset (condition-case nil
                           (-> (org-glance? view :location)
                               (org-glance-view:locate-header)
                               (org-glance-view:read-header)
                               (org-glance? :offset))
                         (file-missing (org-glance-offset:zero))))
        (memory-offset (org-glance? view :offset)))
    (-min-by #'org-glance-offset:less? (list buffer-offset memory-offset))))

(defun org-glance-view:set-offset (view offset)
  (setf (org-glance? view :offset) offset)
  (org-glance-view:save-header view))

(cl-defun org-glance-view:marker-at-point (&optional
                                             (view (org-glance-view:current))
                                             (point (point)))
  (org-glance-vector:non-binary-search (org-glance? view :markers) point))

(org-glance-declare org-glance-view:shift-markers! :: org-glance-view -> number -> number -> (org-glance-list-of number))
(defun org-glance-view:shift-markers! (view midx diff)
  (cl-loop with asterisk = "\n*"
     with markers = (org-glance? view :markers)
     for idx from (1+ midx) below (org-glance-vector:size markers)
     for marker = (org-glance-vector:get markers idx)
     for marker-position = (+ (org-glance-view:get-marker-position view idx) diff)
     for asterisk* = (condition-case nil
                         (buffer-substring-no-properties (1- marker-position) (1+ marker-position))
                       (error ""))
     do (org-glance-view:set-marker-position view idx marker-position)
     when (or (< marker-position (point-min))
              (< (point-max) marker-position)
              (not (string= asterisk asterisk*)))
     do (org-glance! marker :removed? := t)
     when (and (>= marker-position (point-min))
               (>= (point-max) marker-position)
               (string= asterisk asterisk*))
     do (org-glance! marker :removed? := nil)))

(org-glance-declare org-glance-view-header:from-view :: View -> ViewHeader)
(defun org-glance-view-header:from-view (view)
  "Get compact metadata for VIEW."
  (org-glance-view-header
   :type (org-glance? view :type)
   :offset (org-glance? view :offset)
   :size (org-glance-vector:size (org-glance? view :markers))))

(org-glance-declare org-glance-view:save-header :: View -> t)
(defun org-glance-view:save-header (view)
  "Save VIEW header."
  (org-glance:with-temp-file (org-glance-view:locate-header (org-glance? view :location))
    (insert (pp-to-string (org-glance-view-header:from-view view)))))

(org-glance-declare org-glance-view:read-header :: ReadableFile -> ViewHeader)
(defun org-glance-view:read-header (file)
  "Read header from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(org-glance-declare org-glance-view:locate-header :: OptionalFile -> OptionalFile)
(defun org-glance-view:locate-header (view-location)
  "Determine header location by VIEW-LOCATION."
  (thread-first view-location
    (file-name-sans-extension)
    (concat org-glance-view-header-extension)))

(org-glance-declare org-glance-view:locate-markers :: View -> OptionalFile)
(defun org-glance-view:locate-markers (view)
  "Determine location of markers by VIEW."
  (thread-first view
    (org-glance? :location)
    (file-name-sans-extension)
    (concat org-glance-view-marker-extension)))

(defun org-glance-view:freeze-markers (view)
  (cl-loop with result = (make-hash-table :test #'equal)
     for hash being the hash-keys of (org-glance? view :hash->midx) using (hash-values midx)
     do (puthash hash (org-glance-view:get-marker-headline view midx) result)
     finally return result))

(provide 'org-glance-view-model)
;;; org-glance-view-model.el ends here
