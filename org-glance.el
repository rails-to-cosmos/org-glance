;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0.0

;; Keywords: org-mode tools
;; Homepage: https://github.com/rails-to-cosmos/org-glance

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'a)
(require 'bindat)
(require 'cl-generic)
(require 'cl-macs)
(require 'dash)
(require 'eieio)
(require 'f)
(require 'files)
(require 'nadvice)
(require 'ol)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 's)
(require 'thunk)

(defalias 's-replace-regexp 'replace-regexp-in-string)

(defconst org-glance-log:loggers
  (a-list :events nil
          :world nil
          :headlines nil
          :cache nil
          :dimensions nil
          :contents t
          :markers t
          :buffers nil
          :performance nil
          :offsets nil
          :test nil
          :changelog nil))

(defvar org-glance-current-world nil
  "Current instance of `org-glance-world'.")

(defconst org-glance-scope-extensions '("org" "org_archive"))

(defconst org-glance-vector:default-capacity 64)

(defconst org-glance-encrypted-re "aes-encrypted V [0-9]+.[0-9]+-.+\n"
  "Encrypted header pattern.")

(defconst org-glance-user-property-1-re
  "[[:blank:]]+\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(defconst org-glance-user-property-2-re
  "^\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(defvar org-glance-scope:default-scope-alist
  '((file-with-archives . org-glance-scope:file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance:agenda-with-archives)))

(cl-defun lance-sub (type)
  "Substitute lance's type declarations with full elisp declarations."
  (pcase type
    ((and T (cl-struct symbol) (guard (<= 65 (aref (symbol-name T) 0) 90)))
     (read (format "org-glance-%s"
                   (let ((case-fold-search nil))
                     (downcase (s-replace-regexp "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" (symbol-name T)))))))
    ((and T (cl-struct list)) (-map #'lance-sub T))
    (_ type)))

(cl-defmacro lance-dec (name _ &rest types)
  "Declare TYPES for function NAME."
  (let ((symbol (lance-sub name)))
    `(progn
       ,(when-let (arg-types (-butlast types))
          `(advice-add (quote ,symbol)
                       :before (lambda (&rest args)
                                 ,(cl-loop for type in arg-types
                                     for i from 0
                                     when (not (eq '-> type))
                                     collect `(cl-check-type (nth ,(/ i 2) args) ,(lance-sub type))
                                     into typechecks
                                     finally return (append '(progn) typechecks)))))
       (advice-add (quote ,symbol)
                   :filter-return (lambda (result)
                                    ,(when-let (return-type (car (last types)))
                                       `(cl-the ,(lance-sub return-type) result)))))))

(cl-defmacro lance-def (name &rest body)
  (declare (indent defun))
  `(cl-defun ,(lance-sub name) ,@body))

(cl-defmacro lance-run (name &rest args)
  `(,(lance-sub name) ,@args))

(cl-defmacro lance-progn (&rest args)
  `(progn ,@(lance-sub args)))

(cl-defmacro org-glance-fun (name args _ return-type &rest body)
  (declare (indent 4))
  (cl-loop
     for (arg _ type) in args
     collect arg into cl-args
     collect (list arg type) into cl-types
     finally return (append `(cl-defun ,name)
                            (list cl-args)
                            (cl-loop for (arg type) in cl-types
                               collect `(cl-check-type ,arg ,(lance-sub type)))
                            (list `(cl-the ,(lance-sub return-type)
                                     (progn ,@body))))))

(defmacro lance-get (object &rest slots)
  "Get mutable pointers from SLOTS recursively starting from OBJECT.

Example: (lance-get world :view :markers [0] :hash)"
  (declare (indent 1))
  (cl-reduce
   (lambda (acc slot)
     (let ((s-slot (format "%s" slot)))
       (cond
         ;; convert :-prefixed symbols to slot names
         ((s-starts-with? ":" s-slot) `(slot-value ,acc ,slot))
         ;; convert other symbols to array indices
         ((and (s-starts-with? "[" s-slot) (s-ends-with? "]" s-slot))
          (let ((idx (read (substring s-slot 1 (- (length s-slot) 1)))))
            `(cl-typecase ,acc
               (org-glance-vector (aref (slot-value ,acc :array) ,idx))
               (otherwise (aref ,acc ,idx)))))
         (t (user-error "Unknown slot reference: %s" slot)))))
   slots
   :initial-value object))

(defmacro lance-set (object &rest slots)
  (let ((get-slots (--take-while (not (eq it :=)) slots))
        (set-slots (cdr (--drop-while (not (eq it :=)) slots))))
    (pcase set-slots
      (`() (user-error "Set slots should contain more than 1 element"))
      (`(,value) `(setf (lance-get ,object ,@get-slots) ,value))
      (value `(setf (lance-get ,object ,@get-slots) (lance-get ,@value))))))

(defmacro org-glance++ (object &rest slots)
  `(cl-incf (lance-get ,object ,@slots)))

(defmacro org-glance-- (object &rest slots)
  `(cl-decf (lance-get ,object ,@slots)))

(defmacro org-glance-class (name superclasses slots &rest options-and-doc)
  "`defclass' wrapper that avoids compile-time slot declaration warnings."
  (declare (indent 3))
  `(progn
     (eieio-declare-slots ,@(mapcar (lambda (slot) (intern (format ":%s" (car slot)))) slots))
     (defclass ,name ,superclasses ,slots ,@options-and-doc)))

(cl-defmacro org-glance-log:with-logger (logger &rest forms)
  (declare (indent 2))
  `(let ((result (format ,@forms)))
     (with-current-buffer (get-buffer-create (format "*org-glance-log%s*" ,logger))
       (goto-char (point-max))
       (insert (format-time-string "%H:%M:%S+%6N") " " result "\n"))))

(cl-defmacro org-glance-log (logger &rest args)
  (declare (indent 2))
  (if (a-get org-glance-log:loggers logger)
      (pcase logger
        (:performance (let ((value (make-symbol "value"))
                            (start (make-symbol "start"))
                            (gcs (make-symbol "gcs"))
                            (gc (make-symbol "gc")))
                        `(save-match-data
                           (let ((,gc gc-elapsed)
                                 (,gcs gcs-done)
                                 (,start (current-time))
                                 (,value ,@args))
                             (org-glance-log:with-logger ,logger
                                 "[%s] Elapsed time: %fs%s"
                               (caar (quote ,args))
                               (float-time (time-since ,start))
                               (if (> (- gcs-done ,gcs) 0)
                                   (format " (%fs in %d GCs)"
                                           (- gc-elapsed ,gc)
                                           (- gcs-done ,gcs))
                                 ""))
                             ,value))))
        (_ `(save-match-data
              (org-glance-log:with-logger ,logger
                  "[%s] %s"
                ,logger (format ,@args)))))
    (pcase logger
      (:performance `(progn ,@args))
      (_ nil))))

(defun org-glance-scope:file-with-archives ()
  (append (list (buffer-file-name))
          (org-glance-scope:list-file-archives (buffer-file-name))))

(defun org-glance-scope:list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun org-glance-scope:agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (org-glance-scope:list-file-archives filename)))

(cl-defgeneric org-glance-scope (origin)
  "Convert ORIGIN to list of files.")

(cl-defmethod org-glance-scope ((file string))
  "Return list of file S if exists."
  (let ((files (cond ((not (file-exists-p file)) (warn "File \"%s\" does not exist" file) nil)
                     ((not (file-readable-p file)) (warn "File \"%s\" is not readable" file) nil)
                     ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                     ;; Filter files in special modes: `org-glance-material' and `org-glance-overview' files.
                     ;; ((with-temp-buffer
                     ;;    (insert-file-contents file)
                     ;;    (hack-local-variables)
                     ;;    (alist-get 'org-glance-overview-mode (buffer-local-variables))) (warn "File \"%s\" is in `org-glance-overview' mode" file) nil)
                     (t (list file)))))
    (cl-loop for file in files
       when (and (file-equal-p file (file-name-sans-versions file))
                 (member (file-name-extension file) org-glance-scope-extensions))
       collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  "Convert L to flattened list of files."
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  "Return extracted S from `org-glance-scope:default-scope-alist'."
  (if-let (reserved-scope (assoc s org-glance-scope:default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  "Return list of files from buffer B."
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  "Adapt result of F."
  (-some->> f funcall org-glance-scope))

(cl-defun timep (time)
  "If TIME is a Lisp time value then return TIME, else return nil."
  (condition-case nil
      (and (float-time time) time)
    (error nil)))

(cl-defmacro org-glance:with-temp-file (file &rest forms)
  (declare (indent 1))
  `(progn
     (mkdir (file-name-directory ,file) t)
     (with-temp-file ,file
       (org-mode)
       ,@forms)))

(cl-defmacro org-glance:with-file-overwrite (file &rest forms)
  (declare (indent 1))
  `(org-glance:with-temp-file ,file
     (insert-file-contents ,file)
     ,@forms))

(cl-defmacro org-glance:with-temp-buffer (&rest forms)
  `(with-temp-buffer
     (org-mode)
     ,@forms))

(cl-defmacro org-glance-memc (var bodyform handler)
  "Memory consumption report."
  (declare (indent 2))
  `(cl-flet ((memc () (list
                       cons-cells-consed
                       floats-consed
                       vector-cells-consed
                       symbols-consed
                       string-chars-consed
                       intervals-consed
                       strings-consed)))
     (let ((initial-memory-consumption (memc))
           final-memory-consumption)
       (prog1 ,bodyform
         (let ((,var (--map (- (car it) (cdr it)) (-zip (memc) initial-memory-consumption))))
           ,handler)))))

(cl-defmacro org-glance-world:with-locked-location (location &rest forms)
  (declare (indent 1))
  `(when (--> ,location
              (get-file-buffer it)
              (cond ((null it) t)
                    ((buffer-live-p it) (kill-buffer it))))
     (org-glance:with-file-overwrite ,location
       ,@forms)))

;; Types

(lance-progn
 (cl-deftype Optional (tp) `(satisfies (lambda (thing) (or (null thing) (cl-typep thing (quote ,tp))))))
 (cl-deftype BoundedBy (val) `(satisfies (lambda (thing) (and (cl-typep thing 'number) (>= thing 0) (< thing ,val)))))
 (cl-deftype IndexOf (seq) `(satisfies (lambda (thing) (cl-typep thing '(org-glance-bounded-by ,(seq-length seq))))))
 (cl-deftype ListOf (tp)   `(satisfies (lambda (thing) (and (listp thing) (cl-every (lambda (x) (cl-typep x (quote ,tp))) thing)))))
 (cl-deftype OptionalFile () `(satisfies (lambda (location) (or (not (f-exists-p location)) (and (f-readable-p location) (f-file-p location))))))
 (cl-deftype ReadableFile () `(satisfies (lambda (location) (and (f-readable? location) (f-file? location)))))
 (cl-deftype OptionalDirectory () `(satisfies (lambda (location) (or (not (f-exists? location)) (and (f-readable? location) (f-directory? location))))))
 (cl-deftype ReadableDirectory () `(satisfies (lambda (location) (and (f-readable? location) (f-directory? location)))))
 (cl-deftype Hash () 'string)
 (cl-deftype Offset () 'time)
 (cl-deftype WorldLocation () '(satisfies (lambda (location) (and (f-absolute? location) (f-exists? location) (f-directory? location) (f-readable? location) (f-exists? (f-join location "world.md"))))))
 (cl-deftype Scope () '(ListOf ReadableFile)))

(lance-dec CurrentOffset :: Offset)
(lance-def CurrentOffset ()
  (time-convert nil 'list))

(org-glance-class org-glance-vector nil
    ((array    :type vector :initarg :array)
     (size     :type number :initarg :size)
     (capacity :type number :initarg :capacity)))

(org-glance-class org-glance-event ()
    ((offset :type org-glance-offset
             :initarg :offset
             :initform (lance-run CurrentOffset))))

(org-glance-class org-glance-event:PUT (org-glance-event)
    ((headline :type org-glance-headline-header
               :initarg :headline)))

(org-glance-class org-glance-event:RM (org-glance-event)
    ((hash :type org-glance-hash
           :initarg :hash)))

(org-glance-class org-glance-event:UPDATE (org-glance-event)
    ((hash :type org-glance-hash
           :initarg :hash)
     (headline :type org-glance-headline-header
               :initarg :headline)))

(org-glance-class org-glance-event:UPDATE* (org-glance-event)
    ((hash :type org-glance-hash
           :initarg :hash)
     (headline :type org-glance-headline
               :initarg :headline)))

(org-glance-class org-glance-link nil
    ((title :type string
            :initarg :title)
     (org-link :type string
               :initarg :org-link)
     (position :type number
               :initarg :position)))

(org-glance-class org-glance-headline-header ()
    ((hash :type org-glance-hash
           :initarg :hash
           :documentation "Hash of original headline contents.")
     (title :type string
            :initarg :title
            :documentation "Original headline title.")
     (state :type string
            :initarg :state
            :documentation "TODO state of headline.")
     (tags :type list
           :initarg :tags
           :documentation "List of downcased tags.")
     (timestamps :type list
                 :initarg :timestamps)
     (repeated? :type boolean
                :initarg :repeated?
                :documentation "Is the headline repeated?")
     (active? :type boolean
              :initarg :active?
              :documentation "Is the headline active?")
     (commented? :type boolean
                 :initarg :commented?
                 :documentation "Is the headline commented?")
     (archived? :type boolean
                :initarg :archived?
                :documentation "Is the headline archived?")
     (closed? :type boolean
              :initarg :closed?
              :documentation "Is the headline closed?")
     (encrypted? :type boolean
                 :initarg :encrypted?
                 :documentation "Is the headline encrypted?")
     (linked? :type boolean
              :initarg :linked?
              :documentation "Does the headline contain org links?")
     (store? :type boolean
             :initarg :store?
             :documentation "Does the headline contain user properties?"))
  "Limited edition of `org-glance-headline'.")

(org-glance-class org-glance-headline (org-glance-headline-header)
    ((links :type list
            :initarg :links
            :documentation "Links.")
     (contents :type string
               :initarg :contents
               :documentation "Raw contents of headline.")
     (properties :type list
                 :initarg :properties
                 :documentation "Org-mode properties.")
     (store :type list
            :initarg :store
            :documentation "Properties specified by user in headline contents."))
  "Serializable headline with additional features on top of `org-element'.")

;; Vector

(lance-dec CreateVector :: Vector)
(lance-def CreateVector ()
  (org-glance-vector :array (make-vector org-glance-vector:default-capacity nil)
                     :size 0
                     :capacity org-glance-vector:default-capacity))

(cl-defun org-glance-vector:double-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (lance-get vec :array))
         (old-capacity (lance-get vec :capacity))
         (new-capacity (* 2 old-capacity))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below old-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (lance-set vec :array    := new-arr)
    (lance-set vec :capacity := new-capacity)))

(cl-defun org-glance-vector:half-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (lance-get vec :array))
         (new-capacity (/ (lance-get vec :capacity) 2))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below new-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (lance-set vec :array    := new-arr)
    (lance-set vec :size     := (min (lance-get vec :size) new-capacity))
    (lance-set vec :capacity := new-capacity)))

(cl-defun org-glance-vector:enlarge-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (>= (lance-get vec :size) (lance-get vec :capacity))
    (org-glance-vector:double-capacity! vec)))

(cl-defun org-glance-vector:shrink-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (< (* 4 (lance-get vec :size)) (lance-get vec :capacity))
    (org-glance-vector:half-capacity! vec)))

(cl-defun org-glance-vector:push-back! (vec elem)
  (cl-check-type vec org-glance-vector)

  (org-glance-vector:enlarge-maybe! vec)
  (let ((idx (lance-get vec :size)))
    (lance-set vec :array [idx] := elem))
  (org-glance++ vec :size))

(cl-defun org-glance-vector:push-at! (vec idx elem)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-bounded-by (lance-get vec :size)))

  (org-glance-vector:enlarge-maybe! vec)
  (cl-loop for j from (lance-get vec :size) downto (1+ idx)
     do (lance-set vec :array [j] := vec :array [(- j 1)]))
  (lance-set vec :array [idx] := elem)
  (org-glance++ vec :size))

(cl-defun org-glance-vector:remove-at! (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-bounded-by (lance-get vec :size)))

  (cl-loop for j from (1+ idx) below (lance-get vec :size)
     do (lance-set vec :array [(- j 1)] := vec :array [j]))
  (org-glance-- vec :size)
  (org-glance-vector:shrink-maybe! vec))

(cl-defun org-glance-vector:get (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-bounded-by (lance-get vec :size)))

  (lance-get vec :array [idx]))

(cl-defun org-glance-vector:size (vec)
  (cl-check-type vec org-glance-vector)

  (lance-get vec :size))

(cl-defun org-glance-vector:empty? (vec)
  (cl-check-type vec org-glance-vector)

  (= 0 (org-glance-vector:size vec)))

(cl-defun org-glance-vector:clear! (vec)
  (cl-check-type vec org-glance-vector)

  (lance-set vec :size := 0)
  (while (> (lance-get vec :capacity) org-glance-vector:default-capacity)
    (org-glance-vector:shrink-maybe! vec))
  vec)

(cl-defun org-glance-vector:non-binary-search (vec v &key
                                                       (len #'(lambda (vec) (org-glance-vector:size vec)))
                                                       (key #'(lambda (vec idx) (lance-get (org-glance-vector:get vec idx) :position)))
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

;; Offset

(defalias 'org-glance-offset:equal-p #'time-equal-p)
(defalias 'org-glance-offset:less? #'time-less-p)

(cl-defun org-glance-offset:read (s)
  (time-convert (read s) 'list))

(cl-defun org-glance-offset:zero ()
  (cl-the org-glance-offset
    '(0 0 0 0)))

;; Headline

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-match-data
     (save-excursion
       (when (and (org-glance-headline:back-to-headline)
                  (org-at-heading-p))
         (save-restriction
           (narrow-to-region
            (save-excursion (org-back-to-heading t) (point))
            (save-excursion (org-end-of-subtree t t)))
           ,@forms)))))

(cl-defun org-glance-headline-header:from-headline (headline)
  "Infer instance of `org-glance-headline-header' from HEADLINE."
  (cl-check-type headline (or org-glance-headline-header org-glance-headline))

  (cl-typecase headline
    (org-glance-headline (org-glance-headline-header
                          :hash (lance-get headline :hash)
                          :title (lance-get headline :title)
                          :state (lance-get headline :state)
                          :tags (lance-get headline :tags)
                          :timestamps (lance-get headline :timestamps)
                          :active? (lance-get headline :active?)
                          :repeated? (lance-get headline :repeated?)
                          :commented? (lance-get headline :commented?)
                          :archived? (lance-get headline :archived?)
                          :closed? (lance-get headline :closed?)
                          :encrypted? (lance-get headline :encrypted?)
                          :linked? (lance-get headline :linked?)
                          :store? (lance-get headline :store?)))
    (org-glance-headline-header headline)))

(cl-defun org-glance-ast:get-buffer-ast ()
  (thunk-let* ((subtree (org-element-contents (org-element-parse-buffer)))
               (element (car subtree))
               ;; get offset of the topmost element:
               (indent-offset (1- (org-element-property :level element))))
    ;; Consider side-effect on subtree: we change indentation levels of all nested subtrees
    (when (> indent-offset 0)
      (cl-loop for headline in (org-element-map subtree 'headline #'identity)
         for level = (org-element-property :level headline)
         do (org-element-put-property headline :level (- level indent-offset))))
    subtree))

(cl-defun org-glance-headline:extract-store (contents)
  (cl-loop for (_ key value)
     in (append (s-match-strings-all org-glance-user-property-1-re contents)
                (s-match-strings-all org-glance-user-property-2-re contents))
     when (not (member key org-special-properties))
     collect (cons key value) into result
     finally return (seq-uniq result #'(lambda (a b) (and (string= (car a) (car b))
                                                     (string= (cdr a) (cdr b)))))))

(cl-defun org-glance-ast:contents (ast)
  (->> ast
       org-element-interpret-data
       substring-no-properties
       s-trim))

(cl-defun org-glance-ast:substitute-links-with-titles (ast)
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance-ast:title (ast)
  (with-temp-buffer
    (insert (or (org-element-property :TITLE (car ast))
                (org-element-property :raw-value (car ast))
                ""))
    (->> (org-element-parse-buffer)
         org-glance-ast:substitute-links-with-titles
         org-element-interpret-data
         substring-no-properties
         s-trim)))

(cl-defun org-glance-ast:state (contents)
  (org-glance:with-temp-buffer
   (insert contents)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (-some->> (org-get-todo-state)
     substring-no-properties
     upcase)))

(cl-defun org-glance-ast:hash (contents)
  (->> contents
       s-trim
       downcase
       (replace-regexp-in-string "[[:space:][:blank:][:cntrl:]]+" " ")
       (secure-hash 'md5)))

(cl-defun org-glance-ast:class (ast)
  (-map #'downcase (org-element-property :tags (car ast))))

(cl-defun org-glance-ast:commented? (ast)
  (not (null (org-element-property :commentedp (car ast)))))

(cl-defun org-glance-ast:closed? (ast)
  (not (null (org-element-property :closed (car ast)))))

(cl-defun org-glance-ast:archived? (ast)
  (not (null (org-element-property :archivedp (car ast)))))

(cl-defun org-glance-ast:encrypted? (contents)
  (not (null (s-match-strings-all org-glance-encrypted-re contents))))

(cl-defun org-glance-ast:linked? (contents)
  (not (null (s-match-strings-all org-link-any-re contents))))

(cl-defun org-glance-headline:extract-links ()
  (cl-loop for element in (org-element-map (org-element-parse-buffer) 'link #'identity)
     collect (org-glance-link
              :title (substring-no-properties
                      (or (-some->> element
                            org-element-contents
                            org-element-interpret-data)
                          (org-element-property :raw-link element)))
              :org-link (s-trim (buffer-substring-no-properties
                                 (org-element-property :begin element)
                                 (org-element-property :end element)))
              :position (org-element-property :begin element))))

(cl-defun org-glance-headline-at-point ()
  "Create `org-glance-headline' instance from `org-element' at point."
  (cl-the (org-glance-optional org-glance-headline)
    (save-excursion
      (unless (org-before-first-heading-p)
        (org-glance-headline:with-headline-at-point
          (let* ((ast (org-glance-ast:get-buffer-ast))
                 (contents (org-glance-ast:contents ast))
                 (store (org-glance-headline:extract-store contents))
                 (links (org-glance-headline:extract-links))
                 (timestamps (append (org-element-map ast '(headline) #'(lambda (headline) (org-element-property :scheduled headline)))
                                     (org-element-map ast '(headline) #'(lambda (headline) (org-element-property :deadline headline)))
                                     (org-element-map ast '(timestamp) #'identity))))
            (org-glance-headline
             :title (org-glance-ast:title ast)
             :state (or (org-glance-ast:state contents) "")
             :hash (org-glance-ast:hash contents)
             :tags (org-glance-ast:class ast)
             :active? (not (null (--filter (member (org-element-property :type it) '(active active-range)) timestamps)))
             :repeated? (not (null (--filter (and (member (org-element-property :type it) '(active active-range))
                                                  (> (or (org-element-property :repeater-value it) 0) 0))
                                             timestamps)))
             :timestamps (--map (org-element-property :raw-value it)
                                (--filter (member (org-element-property :type it) '(active active-range)) timestamps))
             :commented? (org-glance-ast:commented? ast)
             :archived? (org-glance-ast:archived? ast)
             :closed? (org-glance-ast:closed? ast)
             :encrypted? (org-glance-ast:encrypted? contents)
             :linked? (not (null links))
             :links links
             :store? (not (null store))
             :store store
             :contents contents
             :properties (org-entry-properties))))))))

(cl-defun org-glance-headline-from-string (string)
  "Create `org-glance-headline' from string."
  (org-glance:with-temp-buffer
   (insert string)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (org-glance-headline-at-point)))

(cl-defun org-glance-headline:save (headline dest)
  "Write HEADLINE to DEST."
  (cond ;; ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and is not empty."))
    ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (org-glance:with-temp-file dest
    (org-glance-headline:insert headline))
  headline)

(cl-defun org-glance-headline:with-properties (headline properties)
  (declare (indent 1))
  (cl-check-type headline org-glance-headline)
  (cl-the org-glance-headline
    (org-glance:with-temp-buffer
     (org-glance-headline:insert headline)
     (cl-loop for (key value) in properties
        do (org-set-property key value))
     (org-glance-headline-at-point))))

(cl-defun org-glance-headline:insert (headline)
  (cl-check-type headline org-glance-headline)
  (insert (lance-get headline :contents) "\n"))

;; (defvar org-glance-headline--bindat-spec
;;   '((title str 255)
;;     (archived? byte)
;;     (commented? byte)
;;     (closed? byte)
;;     (encrypted? byte)
;;     (linked? byte)
;;     (store? byte)))

;; (cl-defmethod org-glance-headline:pack ((headline org-glance-headline))
;;   "Pack HEADLINE according to `org-glance-headline--bindat-spec'."
;;   (cl-flet ((bool->int (bool) (if (null bool) 0 1)))
;;     (bindat-pack
;;      org-glance-headline--bindat-spec
;;      (a-list 'title (string-as-unibyte (lance-get headline :title))
;;              'archived (bool->int (lance-get headline :archived?))
;;              'commented (bool->int (lance-get headline :commented?))
;;              'closed (bool->int (lance-get headline :closed?))
;;              'encrypted (bool->int (lance-get headline :encrypted?))
;;              'linked (bool->int (lance-get headline :linked?))
;;              'store (bool->int (lance-get headline :store?))))))

;; (cl-defmethod org-glance-headline:unpack (bindat-raw)
;;   (bindat-unpack org-glance-headline--bindat-spec bindat-raw))

;; (cl-defmacro comment (&rest forms)
;;   nil)

;; (comment
;;  (f-write-bytes (org-glance-headline:pack (org-glance-headline-at-point)) "/tmp/headline.bin")
;;  (a-get (bindat-unpack org-glance-headline--bindat-spec (f-read-bytes "/tmp/headline.bin")) 'file)
;;  (string-make-unibyte "hello"))

(cl-defmacro org-glance-headline:map (var &rest forms)
  "Map buffer headlines and execute FORMS on each binding headline to VAR."
  (declare (indent 1))
  `(save-excursion
     (cl-loop
        initially do (progn
                       (goto-char (point-min))
                       (unless (org-at-heading-p)
                         (outline-next-heading)))
        while (org-at-heading-p)
        collect (save-excursion
                  (let ((,(car var) (org-glance-headline-at-point)))
                    (org-glance-headline:with-headline-at-point ,@forms)))
        into result
        when (condition-case nil
                 (outline-forward-same-level 1)
               (error t))
        return result)))

(cl-defun org-glance-headline:read (file)
  "Load headline from FILE."
  (org-glance:with-temp-buffer
   (insert-file-contents file)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (org-glance-headline-at-point)))

(cl-defun org-glance-headline:back-to-headline ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

;; Dimension

(org-glance-class org-glance-dimension nil
    ((name :type symbol :initarg :name)
     (form :type list   :initarg :form)))

(org-glance-class org-glance-partition nil
    ((dimension :type string :initarg :dimension)
     (value     :type string :initarg :value)))

(cl-defun org-glance-partition:representation (partition)
  (cl-check-type partition org-glance-partition)

  (downcase (format "%s=%s"
                    (lance-get partition :dimension)
                    (lance-get partition :value))))

(cl-defun org-glance-partition:path (partition)
  (cl-check-type partition org-glance-partition)

  (f-join (lance-get partition :dimension)
          (lance-get partition :value)))

(cl-defun org-glance-partition:from-string (s)
  (cl-check-type s string)

  (cl-destructuring-bind (dimension value)
      (--> s
           (downcase it)
           (s-split-up-to "=" it 2))
    (org-glance-partition :dimension dimension
                          :value value)))

(cl-defun org-glance-partition:from-key-value (k v)
  (org-glance-partition:from-string (downcase (format "%s=%s" k v))))

(cl-defun org-glance-dimension:apply (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline org-glance-headline-header)

  (let ((result (eval (lance-get dimension :form) (a-list 'headline headline))))
    (--map (thread-last it
             (format "%s")
             (downcase)
             (replace-regexp-in-string "[[:blank:][:punct:]]+" "-")
             (replace-regexp-in-string "[[:cntrl:]]+" "")
             (replace-regexp-in-string "[[:nonascii:]]+" "*"))
           (cl-typecase result
             (atom (list result))
             (otherwise result)))))

(cl-defun org-glance-dimension:partitions (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cons (lance-get dimension :name) (org-glance-dimension:apply dimension headline)))

(cl-defun org-glance-dimension:make-predicate (dimension value)
  (let ((name (lance-get dimension :name)))
    (cl-typecase value
      (symbol `(member (quote ,value) ,name))
      (t `(member ,value ,name)))))

(cl-defun org-glance-dimension:predicates (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cl-destructuring-bind (_ &rest partitions) (org-glance-dimension:partitions dimension headline)
    (cl-loop for partition in partitions
       when (not (string-empty-p (s-trim (format "%s" partition))))
       collect (org-glance-dimension:make-predicate dimension partition))))

(cl-defun org-glance-dimension:context (headline dimensions)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))
  (cl-check-type dimensions (org-glance-list-of org-glance-dimension))

  (cl-loop for dimension in dimensions
     collect (org-glance-dimension:partitions dimension headline)))

(lance-dec ValidateDimension :: list -> (or Headline HeadlineHeader) -> (ListOf Dimension) -> (Optional string))
(lance-def ValidateDimension (predicate headline dimensions)
  (pcase (eval predicate (org-glance-dimension:context headline dimensions))
    ((pred (null)) nil)
    (result (format "%s" (car result)))))

;; Changelog

(org-glance-class org-glance-changelog nil
    ((events :type list
             :initarg :events
             :initform nil)
     (test :type function
           :initarg :test
           :initform #'equal)))

(cl-defun org-glance-changelog:flatten (changelog)
  "Return list of LOG events deduplicated."
  (lance-get changelog :events))

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
  `(push ,event (lance-get ,changelog :events)))

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
      (org-glance:with-temp-file location
        (insert contents)))))

(cl-defun org-glance-changelog:merge (lhs rhs)
  (cl-assert (eq (lance-get lhs :test) (lance-get rhs :test)))
  (org-glance-changelog
   :events (append (lance-get lhs :events) (lance-get rhs :events))
   :test (lance-get lhs :test)))

(cl-defun org-glance-changelog:last (changelog)
  (car (lance-get changelog :events)))

(cl-defun org-glance-changelog:filter (changelog func)
  (cl-loop
     for event in (org-glance-changelog:flatten changelog)
     when (funcall func event)
     collect event into events
     finally return (org-glance-changelog
                     :events events
                     :test (lance-get changelog :test))))

(cl-defun org-glance-changelog:length (changelog)
  (length (lance-get changelog :events)))

;; World

(org-glance-class org-glance-world nil
    ((location :type org-glance-world-location :initarg :location :documentation "Directory containing all the data.")
     (changelog* :type org-glance-changelog :initarg :changelog* :initform (org-glance-changelog) :documentation "In-memory changelog.")
     (changelog :type org-glance-changelog :initarg :changelog :initform (org-glance-changelog) :documentation "Persistent changelog.")
     (dimensions :type (org-glance-list-of org-glance-dimension) :initarg :dimensions)

     ;; in-memory caches
     (partitions :type (org-glance-list-of org-glance-partition) :initarg :partitions :initform nil)
     (headline-by-hash :type hash-table :initarg :headline-by-hash :initform (make-hash-table :test #'equal) :documentation "HASH -> HEADLINE")
     (relations :type hash-table :initarg :relations :initform (make-hash-table :test #'equal) :documentation "id -> headlines")))

(lance-dec CreateWorld :: OptionalDirectory -> World)
(lance-def CreateWorld (location)
  (declare (indent 1))
  (f-mkdir-full-path location)
  (f-touch (f-join location "world.md"))
  (org-glance-world :location location))

(lance-dec ReadWorld :: OptionalDirectory -> (Optional World))
(lance-def ReadWorld (location)
  (cl-typecase location
    (org-glance-world-location (let ((world (lance-run CreateWorld location)))
                                 (org-glance-world:read-changelog! world)
                                 (org-glance-world:read-relations! world)
                                 world))
    (otherwise nil)))

(lance-dec WorldOffset :: World -> Offset)
(lance-def WorldOffset (world)
  (pcase (org-glance-changelog:last (lance-get world :changelog))
    ((pred null) (lance-run CurrentOffset))
    (event (lance-get event :offset))))

(lance-dec LocatePartition :: World -> Partition -> OptionalFile)
(lance-def LocatePartition (world partition)
  (f-join (lance-get world :location) "views"
          (org-glance-partition:path partition)
          (format "%s.org" (org-glance-partition:representation partition))))

(lance-dec WorldPartitions :: World -> (ListOf Partition))
(lance-def WorldPartitions (world)
  (or (lance-get world :partitions)
      (lance-set world :partitions := (--map (--> it
                                                    (file-name-sans-extension it)
                                                    (list (file-name-nondirectory (f-parent (f-parent it)))
                                                          (file-name-nondirectory (f-parent it)))
                                                    (-zip-lists '(:dimension :value) it)
                                                    (-flatten it)
                                                    (apply #'org-glance-partition it))
                                               (directory-files-recursively (f-join (lance-get world :location) "views") ".*\\.org$")))))

(lance-dec ReadPartition :: World -> Partition -> list)
(lance-def ReadPartition (world partition)
  (-> (lance-run LocatePartition world partition)
      (org-glance-view:locate-header)
      (org-glance-view:read-header)))

(lance-dec MakePartitions :: World -> Headline -> t)
(lance-def MakePartitions (world headline)
  (cl-loop with dimensions = (lance-get world :dimensions)
     for dimension in dimensions
     for predicates = (org-glance-dimension:predicates dimension headline)
     append (cl-loop for predicate in predicates
               for value = (lance-run ValidateDimension predicate headline dimensions)
               when value
               collect (let* ((partition (org-glance-partition
                                          :dimension (format "%s" (lance-get dimension :name))
                                          :value value))
                              (location (lance-run LocatePartition world partition)))
                         (unless (f-exists? location)
                           (org-glance-log :dimensions "Create derived view %s in %s" partition location)
                           (push partition (lance-get world :partitions))
                           (org-glance-view:get-or-create world partition location (org-glance-offset:zero)))))))

(lance-dec SaveWorld :: World -> Offset)
(lance-def SaveWorld (world)
  "Persist WORLD changes.

- Persist event log.
- Apply PUT operations.
- TODO Apply RM operations.
- ...

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-world' should act like pure
functional data structure.

Return last committed offset."
  (let ((changelog (lance-get world :changelog)))
    (dolist-with-progress-reporter (event (reverse (lance-get world :changelog* :events)))
        (format "Persist world %s" (lance-get world :location))
      (thunk-let* ((source-hash (lance-get event :hash))
                   (target-hash (lance-get event :headline :hash))
                   (headline-exists? (org-glance-world:headline-exists? world target-hash))
                   (event-headline (lance-get event :headline))
                   (headline (org-glance-world:get-headline world target-hash)))
        (cl-typecase event
          (org-glance-event:RM
           (org-glance-world:delete-headline world source-hash)
           (org-glance-changelog:push changelog event))
          (org-glance-event:PUT
           (org-glance-world:write-headline world headline)
           (lance-run MakePartitions world headline)
           (org-glance-changelog:push changelog event))
          (org-glance-event:UPDATE*
           (org-glance-world:write-headline world event-headline)
           (lance-run MakePartitions world event-headline)
           (org-glance-world:delete-headline world source-hash)
           (org-glance-changelog:push changelog (org-glance-event:UPDATE :hash source-hash
                                                                         :headline (org-glance-headline-header:from-headline event-headline))))
          (otherwise (error "Don't know how to handle event of type %s" (type-of event))))))

    (org-glance-world:write-changelog! world)
    (lance-set world :changelog* := (org-glance-changelog))
    (org-glance-world:write-relations! world)

    (if (org-glance-changelog:last changelog)
        (lance-get (org-glance-changelog:last changelog) :offset)
      (lance-run CurrentOffset))))

(org-glance-fun org-glance-world:write-headline ((world :: World) (headline :: Headline)) -> ReadableFile
  "Persist HEADLINE in WORLD."
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location))
    location))

(org-glance-fun org-glance-world:locate-changelog ((world :: World)) -> OptionalFile
  (f-join (lance-get world :location) "log" "event.log"))

(org-glance-fun org-glance-world:write-changelog! ((world :: World)) -> t
  (let ((changelog (lance-get world :changelog))
        (location (org-glance-world:locate-changelog world)))
    (org-glance-changelog:write changelog location)))

(org-glance-fun org-glance-world:read-changelog! ((world :: World)) -> Changelog
  (lance-set world :changelog := (org-glance-changelog:read (org-glance-world:locate-changelog world))))

(org-glance-fun org-glance-world:locate-relations ((world :: World)) -> OptionalFile
  (f-join (lance-get world :location) "relations.el"))

(org-glance-fun org-glance-world:write-relations! ((world :: World)) -> t
  (with-temp-file (org-glance-world:locate-relations world)
    (insert (pp-to-string (lance-get world :relations)))))

(org-glance-fun org-glance-world:read-relations! ((world :: World)) -> t
  (pcase (org-glance-world:locate-relations world)
    ((and (cl-struct org-glance-readable-file) location) (with-temp-buffer
                                                           (insert-file-contents-literally location)
                                                           (lance-set world :relations := (read (buffer-string)))))))

(org-glance-fun org-glance-world:delete-headline ((world :: World)
                                                  (headline :: (or Hash Headline HeadlineHeader))) -> t
  (condition-case nil
      (f-delete (org-glance-world:locate-headline world headline))
    (error nil)))

(org-glance-fun org-glance-world:add-headline! ((world :: World) (headline :: Headline)) -> org-glance-world
  "Put HEADLINE to WORLD."
  (let* ((id (org-glance-world:headline-id world headline))
         (headline (org-glance-headline:with-properties headline
                     `(("GLANCE_ID" ,id)
                       ("DIR" ,(concat "../../../resources/" id))))))
    (org-glance-world:add-headline-to-cache! world id headline)
    (org-glance-changelog:push (lance-get world :changelog*)
      (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))

  world)

(org-glance-fun org-glance-world:add-headline-to-cache! ((world :: World)
                                                         (id :: string)
                                                         (headline :: Headline)) -> Headline
  (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (lance-get headline :title))
  (puthash id t (lance-get world :relations))
  (puthash (lance-get headline :hash) headline (lance-get world :headline-by-hash)))

(org-glance-fun org-glance-world:remove-headline-from-cache! ((world :: World)
                                                              (hash :: Hash)) -> t
  (when-let (headline (gethash hash (lance-get world :headline-by-hash)))
    (org-glance-log :cache "Remove headline \"%s\" from the world cache" (lance-get headline :title))
    (remhash hash (lance-get world :headline-by-hash))))

(org-glance-fun org-glance-world:remove-headline ((world :: World)
                                                  (hash :: Hash)) -> t
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `(lance-run SaveWorld)' method."
  (let ((event (org-glance-event:RM :hash hash))
        (changelog* (lance-get world :changelog*)))
    (org-glance-changelog:push changelog* event))
  ;; Remove headline from cache only on persist
  ;; (org-glance-world:remove-headline-from-cache! world hash)
  )

(org-glance-fun org-glance-world:update-headline ((world :: World)
                                                  (old-hash :: Hash)
                                                  (headline :: Headline)) -> Offset
  "Update HEADLINE with HASH in WORLD."
  (let* (;; (new-hash (lance-get headline :hash))
         (changelog* (lance-get world :changelog*))
         ;; (cache (lance-get world :headline-by-hash))
         (event (org-glance-event:UPDATE* :hash old-hash :headline headline))
         (offset (lance-get event :offset)))
    (org-glance-changelog:push changelog* event)
    ;; TODO remove from cache on persist only
    ;; (puthash new-hash headline cache)
    ;; (remhash old-hash cache)
    offset))

(org-glance-fun org-glance-world:get-headline-from-cache ((world :: World)
                                                          (hash :: Hash)) -> (Optional Headline)
  (gethash hash (lance-get world :headline-by-hash)))

(org-glance-fun org-glance-world:get-headline-from-stage ((world :: World)
                                                          (hash :: Hash)) -> (Optional Headline)
  (cl-loop for event in (org-glance-changelog:flatten (lance-get world :changelog*))
     do (cl-typecase event
          (org-glance-event:RM (pcase hash
                                 ((pred (string= (lance-get event :hash))) (cl-return nil))))
          (org-glance-event:UPDATE (pcase hash
                                     ((pred (string= (lance-get event :hash))) (cl-return nil))
                                     ((pred (string= (lance-get event :headline :hash))) (cl-return (lance-get event :headline)))))
          (org-glance-event:PUT (pcase hash
                                  ((pred (string= (lance-get event :headline :hash))) (cl-return (lance-get event :headline))))))))

(org-glance-fun org-glance-world:get-headline-from-drive ((world :: World)
                                                          (hash :: Hash)) -> (Optional Headline)
  (org-glance:with-temp-buffer
   (org-glance-log :cache "[org-glance-headline] cache miss: \"%s\"" hash)
   (insert-file-contents (org-glance-world:locate-headline world hash))
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (when-let (headline (org-glance-headline-at-point))
     (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (lance-get headline :title))
     (puthash (lance-get headline :hash) headline (lance-get world :headline-by-hash)))))

(org-glance-fun org-glance-world:get-headline ((world :: World)
                                               (hash :: Hash)) -> Headline
  "Return fully qualified `org-glance-headline' by its hash."
  (or (org-glance-world:get-headline-from-cache world hash)
      (org-glance-world:get-headline-from-stage world hash)
      (org-glance-world:get-headline-from-drive world hash)))

(org-glance-fun org-glance-world:events ((world :: World)) -> (ListOf Event)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (lance-get world :changelog*)
    (lance-get world :changelog))))

(org-glance-fun org-glance-world:headlines ((world :: World)) -> (ListOf HeadlineHeader)
  (cl-loop with removed = (make-hash-table :test #'equal)
     for event in (org-glance-world:events world)
     when (cl-typecase event
            ((or org-glance-event:PUT org-glance-event:UPDATE)
             (not (gethash (lance-get event :headline :hash) removed))))
     collect (lance-get event :headline)
     when (cl-typecase event
            ((or org-glance-event:RM org-glance-event:UPDATE) t))
     do (puthash (lance-get event :hash) t removed)))

(org-glance-fun org-glance-world:headline-id ((world :: World)
                                              (headline :: (or Headline HeadlineHeader))) -> string
  (cl-labels ((uniquify (id ids &optional (tryout 0))
                (let ((try (if (> tryout 0)
                               (format "%s_%d" id tryout)
                             id)))
                  (cond ((gethash try ids) (uniquify id ids (1+ tryout)))
                        (t try))))
              (truncate (len s ellipsis)
                (declare (pure t) (side-effect-free t))
                (if (> (length s) len)
                    (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
                  s)))
    (uniquify (s-join "_" (list (format-time-string "%Y-%m-%d")
                                (--> (lance-get headline :title)
                                     (replace-regexp-in-string "[^a-z0-9A-Z_]" "_" it)
                                     (replace-regexp-in-string "\\-+" "-" it)
                                     (replace-regexp-in-string "\\-+$" "" it)
                                     (truncate 50 it ""))))
              (lance-get world :relations))))

(org-glance-fun org-glance-world:locate-headline ((world :: World)
                                                  (headline :: (or Hash
                                                                   Headline
                                                                   HeadlineHeader))) -> OptionalFile
  "Return location of HEADLINE in WORLD."

  (cl-typecase headline
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (lance-get headline :hash)))
    (string (let ((prefix (substring headline 0 2))
                  (postfix (substring headline 2 (length headline))))
              (f-join (lance-get world :location) "data" prefix postfix)))))

(org-glance-fun org-glance-world:headline-exists? ((world :: World)
                                                   (headline (or Hash
                                                                 Headline
                                                                 HeadlineHeader))) -> boolean
  (f-exists? (org-glance-world:locate-headline world headline)))

(org-glance-fun org-glance-world:make-predicate ((world :: World)
                                                 (partition :: Partition)) -> list
  (cl-loop for dimension in (lance-get world :dimensions)
     when (string= (lance-get partition :dimension)
                   (format "%s" (lance-get dimension :name)))
     return (org-glance-dimension:make-predicate dimension (lance-get partition :value))))

(org-glance-fun org-glance-world:validate-headline ((world :: World)
                                                    (partition :: Partition)
                                                    (headline :: HeadlineHeader)) -> (Optional string)
  (let ((predicate (org-glance-world:make-predicate world partition)))
    (lance-run ValidateDimension predicate headline (lance-get world :dimensions))))

(org-glance-fun org-glance-world:get-partition-headlines ((world :: World)
                                                          (partition :: Partition)) -> (ListOf HeadlineHeader)
  (declare (indent 1))
  (let ((predicate (org-glance-world:make-predicate world partition))
        (headlines (org-glance-world:headlines world)))
    (cl-loop for headline in headlines
       when (lance-run ValidateDimension predicate headline (lance-get world :dimensions))
       collect headline)))

(cl-defun org-glance-world:root (location)
  (cl-typecase location
    (org-glance-world-location location)
    (otherwise (org-glance-world:root (f-parent location)))))

(cl-defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (cl-check-type location org-glance-optional-directory)

  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-read-world)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-create-world)))))

(cl-defun org-glance-world:import (world location)
  "Add headlines from LOCATION to WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type location org-glance-readable-directory)

  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline! world headline))))

  world)

(cl-defun org-glance-world:materialize (world &optional (partition (org-glance-world:choose-partition world)))
  (cl-check-type world org-glance-world)
  (cl-check-type partition (org-glance-optional org-glance-partition))

  (cl-typecase partition
    (org-glance-partition (find-file (org-glance-world:updated-partition world partition)))
    (otherwise nil)))

(cl-defun org-glance-world:agenda (world)
  (cl-check-type world org-glance-world)

  (pcase (org-glance-world:choose-partition world)
    ((and (cl-struct org-glance-partition) partition) (progn
                                                        (setq org-agenda-files (list (org-glance-world:updated-partition world partition))
                                                              org-agenda-overriding-header "org-glance agenda"
                                                              org-agenda-start-on-weekday nil
                                                              org-agenda-span 21
                                                              org-agenda-start-day "-7d")
                                                        (org-agenda-list)))
    (_ nil)))

(cl-defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (or (thread-first (buffer-file-name)
        (org-glance-world:root)
        (org-glance-world:get-or-create))
      (user-error "World %s is not registered in the system" (buffer-file-name))))

(cl-defun org-glance-world:after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world:add-headline! world headline))
    (lance-run SaveWorld world)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(cl-defun org-glance-world:capture-location (world)
  (cl-check-type world org-glance-world)

  (f-join (lance-get world :location) "capture.org"))

(cl-defun org-glance-world:capture (world &key
                                            (template "* %?")
                                            (text (cond ((use-region-p) (buffer-substring-no-properties
                                                                         (region-beginning)
                                                                         (region-end)))
                                                        (t "")))
                                            finalize)
  (declare (indent 1))
  (cl-check-type world org-glance-world)

  (let ((file (org-glance-world:capture-location world)))
    (delete-file file)
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-world:after-finalize-hook 0 t)

    (let ((old-capture-templates org-capture-templates))
      (setq org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template)))
      (unwind-protect
           (org-capture nil "_")
        (setq org-capture-templates old-capture-templates))
      (insert text))
    (when finalize
      (org-capture-finalize))))

(cl-defun org-glance-world:choose-headline (world partition)
  "TODO Should be consistent with dimensions."
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (let ((dummies (--map (cons (lance-get it :title) (lance-get it :hash))
                        (org-glance-world:get-partition-headlines world partition))))
    (thread-last (completing-read (format "Choose headline (%s): " (org-glance-partition:representation partition)) dummies)
      (a-get dummies)
      (org-glance-world:get-headline world))))

(cl-defun org-glance-world:jump (world)
  (cl-check-type world org-glance-world)

  (let* ((partition (org-glance-partition:from-string "linked=t"))
         (headline (org-glance-world:choose-headline world partition))
         (links (lance-get headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (lance-get it :title) links))))
                                             (--drop-while (not (string= link-title (lance-get it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (lance-get link :org-link))))

(cl-defun org-glance-world:extract-headline (world)
  (cl-check-type world org-glance-world)

  (let* ((partition (org-glance-partition:from-string "store=t"))
         (headline (org-glance-world:choose-headline world partition))
         (store (lance-get headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

(cl-defun org-glance-world:choose-partition (world &optional dimension)
  (cl-check-type world org-glance-world)
  (cl-check-type dimension (org-glance-optional string))

  (let* ((partitions (cl-typecase dimension
                       (string (--filter (string= (lance-get it :dimension) dimension)
                                         (lance-run WorldPartitions world)))
                       (otherwise (lance-run WorldPartitions world))))
         (reprs (--map (org-glance-partition:representation it) partitions)))
    (when-let (choice (condition-case nil
                          (if reprs
                              (completing-read "Choose partition: " reprs nil t)
                            (user-error "Partitions not found"))
                        (quit nil)))
      (org-glance-partition:from-string choice))))

(cl-defun org-glance-world:updated-partition (world partition)
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (let* ((location (lance-run LocatePartition world partition))
         (header (lance-run ReadPartition world partition))
         (view-type (a-get header :type))
         (view-offset (a-get header :offset))
         ;; (world-offset (org-glance-world:offset world))
         (view (org-glance-view:get-or-create world view-type location view-offset)))

    ;; (when (org-glance-offset:less? view-offset world-offset)
    ;;   )

    (org-glance-world:with-locked-location location
      (org-glance-view:mark! view)
      (org-glance-view:fetch! view)
      (org-glance-view:save-header view))

    location))

(cl-defun org-glance-world:backfill (world)
  (cl-check-type world org-glance-world)

  (dolist-with-progress-reporter (event (org-glance-world:events world))
      "Backfill"
    (thunk-let ((headline (org-glance-world:get-headline world (lance-get event :headline :hash))))
      (when (org-glance-world:headline-exists? world (lance-get event :headline :hash))
        (cl-typecase event
          (org-glance-event:RM nil)
          (org-glance-event:PUT (lance-run MakePartitions world headline))
          (org-glance-event:UPDATE (lance-run MakePartitions world headline)))))))

(defvar org-glance-world--cache (make-hash-table :test #'equal)
  "List of worlds registered in system.")

(cl-defun org-glance-world-cache:get (location)
  (let ((world (gethash (file-truename location) org-glance-world--cache)))
    (if world
        (org-glance-log :cache "[org-glance-world] cache hit: %s" location)
      (org-glance-log :cache "[org-glance-world] cache miss: %s" location))
    world))

(cl-defun org-glance-world-cache:put (world)
  (cl-typecase world
    (org-glance-world (org-glance-log :cache "[org-glance-world] cache put: %s" (lance-get world :location))
                      (puthash (lance-get world :location) world org-glance-world--cache)
                      world)
    (otherwise nil)))

;; View

(defconst org-glance-view--header-extension ".h")
(defconst org-glance-marker-extension ".m")

(declare-function f-mkdir-full-path 'f)

(org-glance-class org-glance-marker nil
    ((hash :type string :initarg :hash)
     (position :type number :initarg :position)
     (changed? :type boolean :initarg :changed? :initform nil)
     (removed? :type boolean :initarg :removed? :initform nil)))

(org-glance-class org-glance-view nil
    ((world
      :type org-glance-world
      :initarg :world
      :documentation "Original `org-glance-world' instance.")
     ;; available TODO states
     ;; capture template
     (type
      :type org-glance-partition
      :initarg :type
      :documentation "Type declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (location
      :type org-glance-optional-file
      :initarg :location
      :documentation "Location where view persists.")
     (offset
      :type org-glance-offset
      :initarg :offset)
     (markers
      :type org-glance-vector
      :initarg :markers
      :initform (lance-run CreateVector)
      :documentation "Dynamic array with headlines.")
     (hash->midx
      :type hash-table
      :initarg :hash->midx
      :initform (make-hash-table :test #'equal)
      :documentation "Hash to idx.")))

(cl-defun org-glance-view:create (world type location offset)
  "Create `org-glance-view' instance from WORLD by TYPE and store it in LOCATION with initial OFFSET."
  (let ((view (org-glance-view :world world
                               :type type
                               :location location
                               :offset offset)))
    (unless (f-exists? (lance-get view :location))
      (f-mkdir-full-path (f-parent (lance-get view :location)))
      (org-glance-view:save-header view)
      (org-glance:with-temp-file location
        (insert (s-join "\n"
                        (list "#  -*- mode: org; mode: org-glance-material -*-"
                              ""
                              "#+STARTUP: overview"
                              ;; (format "#+TYPE: %s :: %s"
                              ;;         (lance-get view :world :location)
                              ;;         (cl-prin1-to-string (lance-get view :type)))
                              ;; (format "#+OFFSET: %s"
                              ;;         (lance-get view :offset))
                              ;; (format "#+PROPERTY: ATTACH_DIR ./../../resources/%s/%s/"
                              ;;         (thread-first (lance-get view :location)
                              ;;           f-parent
                              ;;           file-name-nondirectory
                              ;;           downcase)
                              ;;         (thread-first (lance-get view :location)
                              ;;           file-name-nondirectory
                              ;;           file-name-sans-extension
                              ;;           downcase))
                              ""
                              "")))))
    view))

(cl-defmacro org-glance-view:if-safe-marker (view midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (org-glance-vector:size (lance-get ,view :markers)))
       ,then
     ,@else))

(cl-defun org-glance-view:get-marker-headline (view midx)
  (org-glance-view:if-safe-marker view midx
      (save-excursion
        (goto-char (org-glance-view:get-marker-position view midx))
        (org-glance-headline-at-point))))

(cl-defun org-glance-view:get-marker-index (view hash)
  (gethash hash (lance-get view :hash->midx)))

(cl-defun org-glance-view:get-marker-position (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-get marker :position))
    (point-max)))

(cl-defun org-glance-view:set-marker-position (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-set marker :position := val))))

(cl-defun org-glance-view:set-marker-remove (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-set marker :position := val))))

(cl-defun org-glance-view:set-marker-hash (view midx val)
  (org-glance-view:if-safe-marker view midx
      (progn
        (remhash (org-glance-view:get-marker-hash view midx) (lance-get view :hash->midx))
        (puthash val midx (lance-get view :hash->midx))
        (lance-set view :markers [midx] :hash := val))))

(cl-defun org-glance-view:get-marker-hash (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-get marker :hash))))

(cl-defun org-glance-view:marker-changed? (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-get marker :changed?))))

(cl-defun org-glance-view:set-marker-changed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (lance-get view :markers) midx)))
        (lance-set marker :changed? := val))))

(cl-defmacro org-glance-view:with-current-buffer (view &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (lance-get ,view :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

(cl-defun org-glance-view:add-headline (view headline)
  (cl-check-type view org-glance-view)
  (cl-check-type headline org-glance-headline)

  (let* ((markers (lance-get view :markers))
         (hash (lance-get headline :hash))
         (marker (org-glance-marker :hash hash :position (point-max))))
    (org-glance-vector:push-back! markers marker)
    (goto-char (point-max))
    (org-glance-headline:insert headline)
    (puthash hash (- (org-glance-vector:size markers) 1) (lance-get view :hash->midx))))

(cl-defun org-glance-view:remove-headline (view hash)
  (cl-check-type view org-glance-view)
  (cl-check-type hash org-glance-hash)

  (org-glance-view:with-current-buffer view
    (let* ((midx (org-glance-view:get-marker-index view hash))
           (mpos (org-glance-view:get-marker-position view midx))
           (markers (lance-get view :markers)))
      (goto-char mpos)
      (org-glance-headline:with-headline-at-point
        (let ((inhibit-modification-hooks t))
          (delete-region (point-min) (point-max))
          (org-glance-vector:remove-at! markers midx)
          (remhash hash (lance-get view :hash->midx)))))))

(cl-defun org-glance-view:replace-headline (view old-hash headline)
  (cl-check-type view org-glance-view)
  (cl-check-type old-hash org-glance-hash)
  (cl-check-type headline org-glance-headline)

  (thunk-let* ((new-hash (lance-get headline :hash))
               (midx (gethash old-hash (lance-get view :hash->midx)))
               (marker-position (org-glance-view:get-marker-position view midx)))
    (goto-char marker-position)
    (org-glance-headline:with-headline-at-point
      (let ((inhibit-message t)
            (org-log-state-notes-into-drawer nil)
            (org-log-into-drawer nil)
            (org-log-note-state nil)
            (org-todo-log-states nil)
            (org-log-done nil))
        (org-edit-headline (lance-get headline :title))
        (org-todo (lance-get headline :state))
        (when (lance-get headline :commented?)
          (org-toggle-comment))
        (org-set-tags (lance-get headline :tags)))

      (goto-char (point-min))
      (when (= 0 (forward-line))
        (delete-region (point) (point-max)))
      (goto-char (point-max))

      (insert (with-temp-buffer
                (insert (lance-get headline :contents))
                (goto-char (point-min))
                (forward-line)
                (buffer-substring-no-properties (point) (point-max))))

      (unless (string= (buffer-substring-no-properties (1- (point-max)) (point-max)) "\n")
        (insert "\n"))

      (org-glance-view:set-marker-hash view midx new-hash))))

(cl-defun org-glance-view:make-markers ()
  (let ((markers (lance-run CreateVector)))
    (org-glance-log :cache "[org-glance-view:mark] cache miss: make markers")
    (org-glance-headline:map (headline)
      (let ((marker (org-glance-marker :hash (lance-get headline :hash)
                                       :position (point-min))))
        (org-glance-vector:push-back! markers marker)))
    markers))

(cl-defun org-glance-view:set-markers! (view markers)
  (cl-check-type view org-glance-view)
  (cl-check-type markers org-glance-vector)

  (cl-loop
     with hash->midx = (make-hash-table :test #'equal)
     for midx below (org-glance-vector:size markers)
     for marker = (org-glance-vector:get markers midx)
     do (puthash (lance-get marker :hash) midx hash->midx)
     finally do (setf (lance-get view :markers) markers
                      (lance-get view :hash->midx) hash->midx)))


(cl-defun org-glance-view:mark! (view)
  "Create effective representation of VIEW headline positions."
  (cl-check-type view org-glance-view)

  (pcase (org-glance-view:load-markers view)
    ('() (let ((markers (org-glance-view:make-markers)))
           (org-glance-view:set-markers! view markers)
           (org-glance-view:save-markers view)))
    (markers
     (org-glance-view:set-markers! view markers))))

(cl-defun org-glance-view:commit (&optional (view (org-glance-view:get-buffer-view)))
  (org-glance-view:with-current-buffer view
    (cl-loop with markers = (lance-get view :markers)
       with world = (lance-get view :world)
       with to-move = '()  ;; hashes to move from current view to another view
       with to-remove = '()  ;; hashes to remove from current world
       for midx from 0 below (org-glance-vector:size markers)
       when (and (lance-get view :markers [midx] :changed?)
                 (not (lance-get view :markers [midx] :removed?)))
       do (let* ((headline (org-glance-view:get-marker-headline view midx))
                 (old-hash (org-glance-view:get-marker-hash view midx))
                 (new-hash (lance-get headline :hash)))

            (org-glance-world:update-headline world old-hash headline)
            (lance-set view :markers [midx] :changed? := nil)
            (org-glance-view:set-marker-hash view midx new-hash)

            (unless (org-glance-world:validate-headline world (lance-get view :type) headline)
              (push new-hash to-move)))
       when (lance-get view :markers [midx] :removed?)
       do (push midx to-remove)
       finally do
         (dolist (hash to-move)
           (org-glance-view:remove-headline view hash))

         (cl-loop for midx in to-remove
            for offset from 0
            do
              (org-glance-world:remove-headline world (lance-get view :markers [(- midx offset)] :hash))
              (org-glance-vector:remove-at! (lance-get view :markers) (- midx offset)))

         (let ((offset (lance-run SaveWorld world)))
           (org-glance-view:set-offset view offset))
         (org-glance-view:save-markers view))))

(cl-defun org-glance-view:save-markers (view)
  (cl-check-type view org-glance-view)

  (let ((markers (lance-get view :markers))
        (buffer-hash (buffer-hash)))
    (with-temp-file (org-glance-view:locate-markers view)
      (insert (format "%s\n" buffer-hash))
      (cl-loop for midx below (org-glance-vector:size markers)
         for marker = (org-glance-vector:get markers midx)
         do (insert (format "%s %d\n" (lance-get marker :hash) (lance-get marker :position)))))
    markers))

(cl-defun org-glance-view:load-markers (view)
  (cl-check-type view org-glance-view)

  (let ((location (org-glance-view:locate-markers view)))
    (when (f-exists? location)
      (org-glance-view:with-current-buffer view
        (let ((view-hash (buffer-hash)))
          (with-temp-buffer
            (insert-file-contents location)
            (goto-char (point-min))

            (thunk-let ((mark-hash (buffer-substring-no-properties (point) (line-end-position))))
              (cond
                ((string= mark-hash view-hash) (cl-loop with markers = (lance-run CreateVector)
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

(cl-defun org-glance-view:first-known-anchestor (hash      ;; hash
                                                 relations ;; relations
                                                 idx       ;; relation index
                                                 known     ;; hash store
                                                 )
  "Search for the first known anchestor (member of KNOWN) of HASH through RELATIONS starting at IDX."
  (cl-loop with anchestor = hash
     for j from idx downto 0
     for r = (lance-get relations [j])
     for s = (car r)
     for d = (cdr r)
     when (gethash anchestor known)
     return anchestor
     when (string= anchestor d)
     do (setq anchestor s)))

(cl-defun org-glance-view:fetch! (view)
  (cl-check-type view org-glance-view)

  (let* ((world (lance-get view :world))
         (view-offset (org-glance-view:get-offset view))
         (events (reverse (org-glance-world:events world)))
         (relations (make-vector (length events) nil))
         (progress-reporter (make-progress-reporter "Fetching events" 0 (length events)))
         (committed-offset view-offset)
         (to-add (make-hash-table :test #'equal)))

    ;; initial state
    (cl-loop for hash being the hash-keys of (lance-get view :hash->midx) using (hash-values midx)
       do (puthash hash (org-glance-view:get-marker-headline view midx) to-add))

    (cl-loop
       for event in events
       for idx from 0
       for event-offset = (lance-get event :offset)

       when (cl-typep event 'org-glance-event:UPDATE)
       do (aset relations idx (cons (lance-get event :hash) (lance-get event :headline :hash)))

       when (org-glance-offset:less? view-offset event-offset)
       do (thunk-let* ((headline* (lance-get event :headline))

                       (event-hash (lance-get event :hash))
                       (headline-hash (lance-get headline* :hash))
                       (derived-hash (org-glance-view:first-known-anchestor event-hash relations idx to-add))
                       (dimensions (lance-get world :dimensions))

                       (headline (org-glance-world:get-headline world headline-hash))

                       (hashes-equal? (string= headline-hash event-hash))
                       (headline-derived? (string= derived-hash headline-hash))
                       (dimension-valid? (org-glance-world:validate-headline world (lance-get view :type) headline*))
                       (dimension-invalid? (not dimension-valid?))
                       (source-exists? (not (null (gethash event-hash to-add))))
                       (source-removed? (not (f-exists? (org-glance-world:locate-headline world event-hash))))
                       (target-removed? (not (f-exists? (org-glance-world:locate-headline world headline-hash))))

                       (add-target! (puthash headline-hash headline to-add))
                       (remove-source!  (remhash event-hash to-add))
                       (derive-headline!  (progn (puthash headline-hash headline to-add)
                                                 (remhash derived-hash to-add)))
                       (replace-headline! (progn (puthash headline-hash headline to-add)
                                                 (remhash event-hash to-add))))
            (cl-typecase event
              (org-glance-event:UPDATE (cond (hashes-equal? nil)
                                             ((and source-removed? target-removed?) nil)
                                             ((and (not source-removed?) target-removed?) remove-source!)
                                             ((and dimension-invalid? (not source-exists?)) nil)
                                             ((and dimension-invalid? source-exists?) remove-source!)
                                             ((and dimension-valid? source-exists?) replace-headline!)
                                             ((and derived-hash (not headline-derived?)) derive-headline!)
                                             ((not derived-hash) add-target!)))
              (org-glance-event:PUT (cond (target-removed? nil)
                                          (dimension-valid? add-target!)))
              (org-glance-event:RM remove-source!)
              (otherwise (user-error "Don't know how to handle event of type %s" (type-of event)))))
         (setq committed-offset event-offset)
         (progress-reporter-update progress-reporter idx (format " (processed %d events of %d)" idx (length events)))
       finally do
         (progress-reporter-done progress-reporter)
         (goto-char (point-min))
         (outline-next-heading)
         (delete-region (point) (point-max))
         (org-glance-vector:clear! (lance-get view :markers))
         (dolist-with-progress-reporter (headline (hash-table-values to-add))
             "Insert headlines"
           (org-glance-view:add-headline view headline))
         (org-glance-view:set-offset view committed-offset))))

(cl-defun org-glance-view:get-offset (view)
  (let ((buffer-offset (condition-case nil
                           (thread-first (lance-get view :location)
                             (org-glance-view:locate-header)
                             (org-glance-view:read-header)
                             (a-get :offset))
                         (file-missing (org-glance-offset:zero))))
        (memory-offset (lance-get view :offset)))
    (-min-by #'org-glance-offset:less? (list buffer-offset memory-offset))))

(cl-defun org-glance-view:set-offset (view offset)
  (setf (lance-get view :offset) offset)
  (org-glance-view:save-header view))

(cl-defun org-glance-view:marker-at-point (&optional
                                             (view (org-glance-view:get-buffer-view))
                                             (point (point)))
  (org-glance-vector:non-binary-search (lance-get view :markers) point))

(org-glance-fun org-glance-view:shift-markers! ((view :: org-glance-view)
                                                (midx :: number)
                                                (diff :: number)) -> (org-glance-list-of number)
  (cl-loop with asterisk = "\n*"
     with markers = (lance-get view :markers)
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
     do (lance-set marker :removed? := t)
     when (and (>= marker-position (point-min))
               (>= (point-max) marker-position)
               (string= asterisk asterisk*))
     do (lance-set marker :removed? := nil)))

(cl-defun org-glance-view:save-header (view)
  (with-temp-file (org-glance-view:locate-header (lance-get view :location))
    (insert (pp-to-string (a-list
                           :type (lance-get view :type)
                           :offset (lance-get view :offset))))))

(cl-defun org-glance-view:read-header (filename)
  (cl-assert (and (f-exists? filename)
                  (s-ends-with? org-glance-view--header-extension filename)))

  (with-temp-buffer
    (insert-file-contents filename)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-view:locate-header (view-location)
  (thread-first view-location
    (file-name-sans-extension)
    (concat org-glance-view--header-extension)))

(cl-defun org-glance-view:locate-markers (view)
  (cl-check-type view org-glance-view)
  (thread-first view
    (lance-get :location)
    (file-name-sans-extension)
    (concat org-glance-marker-extension)))

(cl-defun org-glance-view:get-or-create (world type location offset)
  "Create symbol `org-glance-view' instance from WORLD by TYPE and store it in LOCATION."
  (cl-check-type world org-glance-world)
  (cl-check-type type org-glance-partition)
  (cl-check-type offset org-glance-offset)

  (thunk-let* ((location (file-truename (f-join (lance-get world :location) location)))
               (m-loc (concat (file-name-sans-extension location) org-glance-marker-extension))
               (key (org-glance-view--key :type type :location location))
               (cached-view (org-glance-view-cache:get key))
               (new-view (org-glance-view:create world type location offset)))
    (cond ((and (f-exists? location) (f-exists? m-loc) cached-view) cached-view)
          (t (org-glance-log :cache "[org-glance-view] cache miss: %s" type)
             (org-glance-view-cache:put new-view)
             new-view))))

(cl-defun org-glance-view:get-buffer-view ()
  (let ((header (thread-first (buffer-file-name)
                  (org-glance-view:locate-header)
                  (org-glance-view:read-header)))
        (world (org-glance-world:current)))
    (org-glance-view:get-or-create world (a-get header :type) (buffer-file-name) (a-get header :offset))))

(defvar org-glance-view--cache (make-hash-table :test #'equal)
  "List of views registered in system.")

(org-glance-class org-glance-view--key nil
    ((type
      :type org-glance-partition
      :initarg :type
      :documentation "Type declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (location
      :type org-glance-optional-file
      :initarg :location
      :documentation "Location where view persists."))
  "Unique key for `org-glance-view'.")

(cl-defun org-glance-view-cache:get (key)
  (cl-check-type key org-glance-view--key)
  (if-let (result (gethash key org-glance-view--cache))
      (prog1 (cl-the org-glance-view result)
        (org-glance-log :cache "[org-glance-view] cache hit: %s" key))
    (org-glance-log :cache "cache miss: %s" key)))

(cl-defun org-glance-view-cache:put (view)
  (cl-check-type view org-glance-view)
  (let ((key (org-glance-view--key :type (lance-get view :type)
                                   :location (lance-get view :location))))
    (org-glance-log :cache "[org-glance-view] cache put: %s" key)
    (puthash key view org-glance-view--cache)))

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (add-hook 'before-change-functions #'org-glance-material-mode:before-update nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode:after-update nil t)
         (add-hook 'before-save-hook #'org-glance-view:commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-view:commit t)
         (remove-hook 'before-change-functions #'org-glance-material-mode:before-update t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:after-update t))))

(cl-defun org-glance-material-mode:before-update (change-beg change-end)
  "Actualize marker overlay."
  (interactive)
  (org-glance-log :markers "Before update change beg: %d" change-beg)
  (org-glance-log :markers "Before update change end: %d" change-end)
  (org-glance-log :markers "Before update marker substring: \"%s\"" (buffer-substring-no-properties change-beg change-end))
  (org-glance-log :contents "Before update contents: \"%s\"" (buffer-string)))

(cl-defun org-glance-material-mode:after-update (change-beg change-end change-len)
  "Actualize marker overlay."
  (interactive)
  (let* ((view (org-glance-view:get-buffer-view))
         (diff (- (- change-end change-beg) change-len))
         (midx (org-glance-view:marker-at-point view (- change-beg 1)))
         (buffer (current-buffer)))
    (org-glance-view:set-marker-changed view midx t)
    (org-glance-view:shift-markers! view midx diff)

    (org-glance-log :markers "After update change beg: %d" change-beg)
    (org-glance-log :markers "After update change end: %d" change-end)
    (org-glance-log :markers "After update change len: %d" change-len)
    (condition-case nil
        (org-glance-log :markers "After update marker substring: \"%s\"" (buffer-substring-no-properties change-beg change-end))
      (error nil))
    (org-glance-log :contents "After update contents: \"%s\"" (buffer-string))
    (org-glance-log :markers "After update markers: %s" (pp-to-string (lance-get view :markers)))

    (save-match-data
      (with-current-buffer (get-buffer-create "*glance-markers*")
        (delete-region (point-min) (point-max))
        (insert (pp-to-string (lance-get view :markers)))
        (insert "\n" (pp-to-string (lance-get view :world :changelog*)))))))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "1.0.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory that contains current `org-glance-world'."
  :group 'org-glance
  :type 'directory)

;; TODO prolog implementation is possible
;; TODO could be implemented in headline header
(defconst org-glance-dimensions
  (list (org-glance-dimension :name 'tag       :form '(lance-get headline :tags))
        (org-glance-dimension :name 'state     :form '(lance-get headline :state))
        (org-glance-dimension :name 'title     :form '(lance-get headline :title))
        (org-glance-dimension :name 'linked    :form '(lance-get headline :linked?))
        (org-glance-dimension :name 'store     :form '(lance-get headline :store?))
        (org-glance-dimension :name 'encrypted :form '(lance-get headline :encrypted?))
        (org-glance-dimension :name 'closed    :form '(lance-get headline :closed?))
        (org-glance-dimension :name 'repeated  :form '(lance-get headline :repeated?))
        (org-glance-dimension :name 'active    :form '(lance-get headline :active?))))

(lance-dec Capture :: t)
(lance-def Capture ()
  (interactive)
  (org-glance-world:capture org-glance-current-world
    ;; :text (apply #'org-link-make-string (org-store-link t t))
    ))

(lance-dec Materialize :: t)
(lance-def Materialize ()
  (interactive)
  (org-glance-world:materialize org-glance-current-world))

(lance-dec Agenda :: t)
(lance-def Agenda ()
  (interactive)
  (org-glance-world:agenda org-glance-current-world))

(lance-dec Jump :: t)
(lance-def Jump ()
  (interactive)
  (org-glance-world:jump org-glance-current-world))

(lance-dec Extract :: t)
(lance-def Extract ()
  (interactive)
  (org-glance-world:extract-headline org-glance-current-world))

(lance-dec Backfill :: t)
(lance-def Backfill ()
  (interactive)
  (org-glance-world:backfill org-glance-current-world))

(lance-dec Import :: ReadableDirectory -> t)
(lance-def Import (location)
  (interactive "DDirectory: ")
  (lance-run SaveWorld (org-glance-world:import org-glance-current-world location)))

(lance-dec Init :: t)
(lance-def Init ()
  "Update system state from `org-glance-directory'."
  (interactive)
  (clrhash org-glance-world--cache)
  (clrhash org-glance-view--cache)
  (let ((world (org-glance-world:get-or-create org-glance-directory)))
    (setf (lance-get world :dimensions) org-glance-dimensions)
    (setq org-glance-current-world world)))

(provide 'org-glance)
;;; org-glance.el ends here
