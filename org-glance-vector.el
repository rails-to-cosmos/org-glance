;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)
(require 'org-glance-types)

(defconst org-glance-vector:DEFAULT-CAPACITY 64)

(org-glance-class org-glance-vector nil
    ((array    :type vector :initarg :array)
     (size     :type number :initarg :size)
     (capacity :type number :initarg :capacity)))

(cl-defun org-glance-vector:create ()
  (let ((capacity org-glance-vector:DEFAULT-CAPACITY))
    (org-glance-vector :array (make-vector capacity nil)
                       :size 0
                       :capacity capacity)))

(cl-defun org-glance-vector:double-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (org-glance? vec :array))
         (old-capacity (org-glance? vec :capacity))
         (new-capacity (* 2 old-capacity))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below old-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (org-glance! vec :array    := new-arr)
    (org-glance! vec :capacity := new-capacity)))

(cl-defun org-glance-vector:half-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (org-glance? vec :array))
         (new-capacity (/ (org-glance? vec :capacity) 2))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below new-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (org-glance! vec :array    := new-arr)
    (org-glance! vec :size     := (min (org-glance? vec :size) new-capacity))
    (org-glance! vec :capacity := new-capacity)))

(cl-defun org-glance-vector:enlarge-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (>= (org-glance? vec :size) (org-glance? vec :capacity))
    (org-glance-vector:double-capacity! vec)))

(cl-defun org-glance-vector:shrink-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (< (* 4 (org-glance? vec :size)) (org-glance? vec :capacity))
    (org-glance-vector:half-capacity! vec)))

(cl-defun org-glance-vector:push-back! (vec elem)
  (cl-check-type vec org-glance-vector)

  (org-glance-vector:enlarge-maybe! vec)
  (let ((idx (org-glance? vec :size)))
    (org-glance! vec :array [idx] := elem))
  (org-glance++ vec :size))

(cl-defun org-glance-vector:push-at! (vec idx elem)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance? vec :size)))

  (org-glance-vector:enlarge-maybe! vec)
  (cl-loop for j from (org-glance? vec :size) downto (1+ idx)
     do (org-glance! vec :array [j] := vec :array [(- j 1)]))
  (org-glance! vec :array [idx] := elem)
  (org-glance++ vec :size))

(cl-defun org-glance-vector:remove-at! (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance? vec :size)))

  (cl-loop for j from (1+ idx) below (org-glance? vec :size)
     do (org-glance! vec :array [(- j 1)] := vec :array [j]))
  (org-glance-- vec :size)
  (org-glance-vector:shrink-maybe! vec))

(cl-defun org-glance-vector:get (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance? vec :size)))

  (org-glance? vec :array [idx]))

(cl-defun org-glance-vector:size (vec)
  (cl-check-type vec org-glance-vector)

  (org-glance? vec :size))

(cl-defun org-glance-vector:empty? (vec)
  (cl-check-type vec org-glance-vector)

  (= 0 (org-glance-vector:size vec)))

(cl-defun org-glance-vector:clear! (vec)
  (cl-check-type vec org-glance-vector)

  (org-glance! vec :size := 0)
  (while (> (org-glance? vec :capacity) org-glance-vector:DEFAULT-CAPACITY)
    (org-glance-vector:shrink-maybe! vec))
  vec)

(cl-defun org-glance-vector:non-binary-search (vec v &key
                                                   (len #'(lambda (vec) (org-glance-vector:size vec)))
                                                   (key #'(lambda (vec idx) (org-glance? (org-glance-vector:get vec idx) :position)))
                                                   (l 0)
                                                       (r (1- (funcall len vec))))
  "Binary search for non-binary persons."
  (declare (indent 2))
  (cl-check-type vec org-glance-vector)
  (cl-check-type v number)

  (thunk-let* ((m (/ (+ l r 1) 2))
               (mv (funcall key vec m))
               (lv (funcall key vec l))
               (rv (funcall key vec r)))
    (cond ((= 0 (funcall len vec)) -1)
          ((< v lv) -1)
          ((= v lv) l)
          ((>= v rv) r)
          ((>= v mv) (org-glance-vector:non-binary-search vec v :l m :r r :key key :len len))
          (t (org-glance-vector:non-binary-search vec v :l l :r (1- m) :key key :len len)))))

(provide 'org-glance-vector)
