;; -*- lexical-binding: t; -*-

(require 'a)
(require 'ts)
(require 'cl-macs)
(require 'cl-lib)

;; Experimental
(require 'thunk)

(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-scope)
(require 'org-glance-log)
(require 'org-glance-event)

(defconst org-glance-store-log-location "wa.log")
(defconst org-glance-store-materializations-filename "materializations.el")

(cl-deftype org-glance-store-event ()
  '(satisfies org-glance-store-event-p))

(cl-defun org-glance-store-event-p (event)
  (and (org-glance-event-p event)
       (org-glance-headline-header-p (org-glance-event-state event))))

(cl-deftype org-glance-store-log ()
  '(satisfies org-glance-store-log-p))

(cl-defun org-glance-store-log-p (log)
  (and (org-glance-log-p log)
       (cl-every #'org-glance-store-event-p (org-glance-log-events log))
       (eq #'org-glance-store:event-id (org-glance-log-id-determinator log))))

(cl-defun org-glance-store:event-id (event)
  (org-glance-headline:hash (org-glance-event-state event)))

(cl-defstruct (org-glance-store
                (:constructor org-glance-store--create)
                (:copier nil))
  "Persistent store of headlines."
  (location nil :type string :read-only t :documentation "Directory where we store all the data.")
  (staged-changes (org-glance-log:create nil #'org-glance-store:event-id)
                  :read-only nil
                  :type org-glance-store-log
                  :documentation "Event log that is not persisted yet.")
  (committed-changes (org-glance-log:create nil #'org-glance-store:event-id)
                     :type org-glance-store-log
                     :documentation "Persistent event log.")
  (lru-cache (make-hash-table :test #'equal) :type hash-table)
  ;; (ts->headline (a-list) :type list :read-only t) ;; interval tree
  )

(cl-defun org-glance-store:create (location)
  "Create store binded to directory LOCATION."
  (cond ((and (f-exists-p location) (not (f-empty-p location)))
         (user-error "Location %s already exists and is not empty" location))
        ((and (f-exists-p location) (not (f-readable-p location)))
         (user-error "Store initialization failed. Location is not readable: %s" location)))
  (let ((store (org-glance-store--create :location location)))
    (org-glance-store:flush store)
    store))

(cl-defun org-glance-store:read (location)
  "Read `org-glance-store' from LOCATION."
  (org-glance-store--create
   :location location
   :committed-changes (org-glance-log:read
                       (f-join location org-glance-store-log-location)
                       #'org-glance-store:event-id)))

(cl-defun org-glance-store-from-scratch (location &rest strings)
  "Simplifies interactive debug. Creates store from LOCATION and puts headlines in it."
  (declare (indent 1))
  (let ((store (org-glance-store:create location)))
    (dolist (string strings)
      (org-glance-store:put store (org-glance-headline-from-string string)))
    store))

(cl-defun org-glance-store/ (store location)
  (apply #'f-join (org-glance-store-location store) (s-split "/" location)))

(cl-defun org-glance-store:flush (store)
  "Persist STORE changes.

- Persist event log.
- Apply PUT operations.
- TODO Apply RM operations.
- ...

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-store' should act like pure
functional data structure."
  (org-glance-log:write (org-glance-store-staged-changes store)
                        (org-glance-store/ store org-glance-store-log-location))
  (while (not (org-glance-log:empty-p (org-glance-store-staged-changes store)))
    (let ((event (org-glance-log:pop (org-glance-store-staged-changes store))))
      (cl-typecase event
        (org-glance-event:RM
         ;; if (org-glance-event:RM-p event)
         ;; do (puthash seen hash)
         ;; TODO think about when to delete headlines
         ;; (f-delete (org-glance-store:headline-location store headline))
         nil)
        (org-glance-event:PUT
         (let* ((headline (org-glance-store:get store (org-glance-store:event-id event)))
                (location (org-glance-store:headline-location store headline)))
           (unless (f-exists-p location)
             (org-glance-headline-save headline location)
             (org-glance-log:push (org-glance-store-committed-changes store) event))))))))

;; TODO `org-glance-log' filter now filter events instead of headlines

(cl-defun org-glance-store:filter (store func)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES filtered by FUNC."
  (let ((filter (lambda (event)
                  (and
                   (org-glance-event:PUT-p event)
                   (funcall func (org-glance-event-state event))))))
    (org-glance-store--create
     :location (org-glance-store-location store)
     :staged-changes (org-glance-log:filter (org-glance-store-staged-changes store) filter)
     :committed-changes (org-glance-log:filter (org-glance-store-committed-changes store) filter))))

;; (cl-defun org-glance-store:filter-normalize (stmt)
;;   "Convert STMT to prefix notation.
;; Each substring is a run of \"valid\" characters, i.e., letters, colons for tags and AND/OR expressions.

;; - :TAG: words wrapped in colons match org-mode tag.
;; - TODO uppercase words match org-mode states.
;; - AND/OR expressions could be used.
;; - Use parentheses to prioritize expressions."
;;   (cl-loop
;;      with stack = (list)
;;      with output = (list)
;;      with operators = (a-list "|" 1
;;                               "&" 2
;;                               "=" 3)
;;      for c across-ref (->> stmt
;;                            (string-replace "&" "")
;;                            (string-replace "|" "")
;;                            (string-replace "=" "")
;;                            (s-replace "and" " & ")
;;                            (s-replace "or" " | ")
;;                            (s-replace-regexp " \\([A-Z]+\\)" "(state = \\1)")
;;                            (s-replace-regexp ":\\([a-zA-Z0-9]+\\):" "(class = \\1)")
;;                            (s-replace-regexp "[ ]+" " ")
;;                            downcase
;;                            s-trim
;;                            (format "(%s)")
;;                            reverse
;;                            (s-replace-all (a-list "(" ")" ")" "(")))
;;      for char = (string c)

;;      if
;;        (s-matches-p "^[:]?[a-zA-Z0-9 ]+[:]?$" char)
;;      do
;;        (push char output)

;;      else if
;;        (string= "(" char)
;;      do
;;        (push char stack)
;;        (push ")" output)

;;      else if
;;        (string= ")" char)
;;      do
;;        (while (and stack (not (string= (car stack) "(")))
;;          (push (concat (pop stack) " ") output)
;;          (push "(" output))
;;        (pop stack) ;; remove '('

;;      else if
;;        (a-get operators char) ;; operator found
;;      do
;;        (while (< (a-get operators char 0)
;;                  (a-get operators (car stack) 0))
;;          (push (concat (pop stack) " ") output)
;;          (push "(" output))
;;        (push char stack)

;;      finally do
;;        (while stack
;;          (push (concat (pop stack) " ") output))

;;      finally return (s-join "" output)))

;; (org-glance-store:filter-normalize ":a1:  AND :b2: OR CANCELLED AND DONE OR TODO")

;; (apply #'string (org-glance-store:filter-tokenize ":a1:  AND :b2: OR CANCELLED"))

;; (-reduce #'list (org-glance-store:filter-tokenize ":a1: AND :b2: OR CANCELLED AND DONE"))
;; (string (aref "aello" 0))

;; (org-glance-store:filter-tokenize ":a1:")

;; (s-match-strings-all "([[:word:][:blank:]:]+)" ":a1: AND :b2: AND (:c3: OR (TODO AND DONE OR :d3:)")
;; (s-match-strings-all ":[[:word:]_]+:" ":a1: AND :b2: AND (:c3: OR (:c2: AND DONE)")

;; (org-glance-store:filter-tokenize "Hello")

(cl-defun org-glance-store:put (store headline)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it.
Append PUT event to WAL and insert headlines to persistent storage.

TODO: Transaction."
  (org-glance-log:push (org-glance-store-staged-changes store)
                       (org-glance-event:PUT (org-glance-headline-header headline)))
  (puthash (org-glance-headline:hash headline) headline (org-glance-store-lru-cache store)))

(cl-defun org-glance-store:remove (store headline)
  "Return `org-glance-store' with HEADLINES removed from STORE.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-store:flush' method."
  (org-glance-log:push (org-glance-store-staged-changes store) (org-glance-event:RM headline))
  (remhash (org-glance-headline:hash headline) (org-glance-store-lru-cache store)))

(cl-defun org-glance-store:get (store hash)
  "Return fully qualified `org-glance-headline' by hash.

1. (TODO) Search in LRU cache.
2. Search in staged changes.
3. Search in persistent storage."
  (or
   (gethash hash (org-glance-store-lru-cache store))
   (cl-loop for event in (org-glance-log:flatten (org-glance-store-staged-changes store))
      for headline = (org-glance-event-state event)
      when (and (org-glance-event:PUT-p event)
                (org-glance-headline-p headline)
                (string= (org-glance-headline:hash headline) hash))
      return headline)
   (org-glance--with-temp-buffer
    (insert-file-contents (org-glance-store:headline-location store hash))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let ((headline (org-glance-headline-at-point)))
      (puthash (org-glance-headline:hash headline)
               headline
               (org-glance-store-lru-cache store))))))

(cl-defun org-glance-store:headlines (store)
  "Return actual headline hashes from STORE."
  (cl-loop
     for event in (org-glance-store:events store)
     when (org-glance-event:PUT-p event)
     collect (org-glance-event-state event)))

(cl-defgeneric org-glance-store:headline-location (store headline)
  "Return location of HEADLINE in STORE.")

(cl-defmethod org-glance-store:headline-location ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store:headline-location store (org-glance-headline:hash headline)))

(cl-defmethod org-glance-store:headline-location ((store org-glance-store) (headline org-glance-headline-header))
  "Return HEADLINE location from STORE."
  (org-glance-store:headline-location store (org-glance-headline:hash headline)))

(cl-defmethod org-glance-store:headline-location ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-store-location store) "data" prefix postfix)))

;; (cl-defun org-glance-store-completing-read (store)
;;   "Read headlines from STORE with completion."
;;   (let* ((title->hash (cl-loop for event in (org-glance-store:events store)
;;                          collect (cons (org-glance-headline-title (org-glance-event-state event))
;;                                        (org-glance-headline:hash (org-glance-event-state event)))))
;;          (hash (alist-get (completing-read "Headline: " title->hash nil t) title->hash nil nil #'string=)))
;;     (org-glance-store:retrieve store hash)))

(cl-defun org-glance-store:events (store)
  (org-glance-log:flatten
   (org-glance-log:merge
    (org-glance-store-staged-changes store)
    (org-glance-store-committed-changes store))))

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (dolist (file (org-glance-scope loc))
    (org-glance--with-temp-buffer
     (insert-file-contents file)
     (org-glance-map (headline) (org-glance-store:put store headline)))))

(cl-defun org-glance-store-equal-p (a b)
  "Return t if A contains same headlines as B.

TODO should be optimized to not read all headlines from store."
  (let ((sorted-a (cl-loop for headline in (org-glance-store:headlines a)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-title it)
                                                     (org-glance-headline-title other))
                                            result)))
        (sorted-b (cl-loop for headline in (org-glance-store:headlines b)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-title it)
                                                     (org-glance-headline-title other)) result))))
    (and (not (null sorted-a))
         (not (null sorted-b))
         (--all? (and (consp it)
                      (org-glance-headline-equal-p (car it) (cdr it)))
                 (-zip sorted-a sorted-b)))))

(provide 'org-glance-store)
