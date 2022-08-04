;; -*- lexical-binding: t; -*-

(require 'ts)
(require 'cl-macs)
(require 'cl-lib)

;; Experimental
(require 'thunk)
(require 'avl-tree)

(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-scope)

(defconst org-glance-store-watermark-filename "watermark.el")
(defconst org-glance-store-wal-filename "wa.log")

(cl-defstruct (org-glance-store (:constructor org-glance-store--create)
                                (:copier nil))
  "Persistent store of headlines."
  (location nil :type string :read-only t :documentation "Directory where we store all the data.")
  (watermark (float-time) :type float :read-only t :documentation "Offset behind which all destructive
   methods were applied to persistent storage.")
  (-title->hash (a-list) :type list :read-only t)
  (-state->hash (a-list) :type list :read-only t)
  (-class->hash (a-list) :type list :read-only t)
  ;; (class->headline (a-list) :type list :read-only t)
  ;; (state->headline (a-list) :type list :read-only t)
  ;; (ts->headline (a-list) :type list :read-only t) ;; interval tree
  ;; (lru-cache (a-list) :type list :read-only t)
  (wal nil :type list :read-only t :documentation "Append-only event log. TODO should be removed from memory store."))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  (cond ((and (f-exists-p location) (f-readable-p location))
         (org-glance-store-read location))
        ((and (f-exists-p location) (not (f-readable-p location)))
         (user-error "Store initialization failed. Location is not readable: %s" location))
        (t (mkdir location t)
           (org-glance-store--create
            :location location
            :watermark (let ((now (float-time)))
                         (with-temp-file (f-join location org-glance-store-watermark-filename)
                           (insert (prin1-to-string now)))
                         now)))))

(cl-defun org-glance-store-from-scratch (location &rest strings)
  "Simplifies interactive debug. Creates store from LOCATION and puts headlines in it."
  (declare (indent 1))
  (let ((store (org-glance-store location)))
    (cl-loop for string in strings
       for headline = (org-glance-headline-from-string string)
       collect headline into headlines
       finally return (apply #'org-glance-store-put-headlines store headlines))))

(cl-defun org-glance-store-read (location)
  "Read `org-glance-store' from LOCATION."
  (cl-loop
     with wal = (org-glance-store-wal-read (f-join location org-glance-store-wal-filename))
     with headlines = (org-glance-store-wal-headlines wal)
     for headline in headlines
     for hash = (org-glance-headline-hash headline)
     for title = (org-glance-headline-title headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     collect (cons state hash) into state->hash
     collect (cons title hash) into title->hash
     append (or (--map (cons it hash) classes) (list (cons nil hash))) into class->hash
     finally return (org-glance-store--create
                     :location location
                     :watermark (cond ((f-exists-p (f-join location org-glance-store-watermark-filename))
                                       (with-temp-buffer
                                         (insert-file-contents-literally (f-join location org-glance-store-watermark-filename))
                                         (read (buffer-substring (point-min) (point-max)))))
                                      ((null wal) (float-time))
                                      (t (cl-destructuring-bind (offset _ _) (car (last wal))
                                           offset)))
                     :-title->hash title->hash
                     :-state->hash state->hash
                     :-class->hash class->hash
                     :wal wal)))

(cl-defun org-glance-store-flush (store)
  "Persist STORE changes. Update watermark.

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-store' should act like pure
functional data structure."
  (cl-loop
     with wal = (reverse (org-glance-store-wal store))
     with watermark = (org-glance-store-watermark store)
     with seen = (make-hash-table :test #'equal)
     for (offset instruction headline) in wal
     while (> offset watermark)
     for hash = (org-glance-headline-hash headline)
     for seen-p = (gethash hash seen)
     when (and (not seen-p) (eq instruction 'RM))
     do (f-delete (org-glance-store-headline-location store headline))
     finally do (cl-destructuring-bind (offset _ _) (car wal)
                  (with-temp-file (f-join (org-glance-store-location store) org-glance-store-watermark-filename)
                    (insert (prin1-to-string offset))))
     finally return store))

(cl-defun org-glance-store-put-headlines (store &rest headlines)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it.

Append PUT event to WAL and insert headlines to persistent storage."
  (cl-loop
     with current-offset = (float-time)
     with title->headline = (make-hash-table :test #'equal)
     with store-location = (org-glance-store-location store)
     with store-watermark = (org-glance-store-watermark store)
     with store-wal = (org-glance-store-wal store)

     for headline in headlines
     for title = (org-glance-headline-title headline)
     for hash = (org-glance-headline-hash headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     for location = (org-glance-store-headline-location store headline)
     for event = (list current-offset 'PUT (org-glance-headline-header headline))

     unless (f-exists-p location) ;; could be made in a separate thread
     ;; no need to write fully qualified headlines, write only headers
     do (org-glance-headline-save headline location)

     ;; uniquify titles
     do (let ((uniq-title title)
              (tryout 1))
          (while (gethash uniq-title title->headline)
            (cl-incf tryout)
            (setq uniq-title (format "%s (%d)" title tryout)))
          (puthash uniq-title hash title->headline))

     collect (cons state hash) into state->hash
     append (or (--map (cons it hash) classes) (list (cons nil hash))) into class->hash

     collect event into wal
     ;; append to wal in persistent storage
     finally do (org-glance-store-wal-append wal (f-join (org-glance-store-location store) org-glance-store-wal-filename))
     ;; return in-memory wal representation
     finally return (org-glance-store--create
                     :location store-location
                     :watermark store-watermark
                     :-title->hash (apply #'a-assoc
                                          (org-glance-store--title->hash store)
                                          (cl-loop for title being the hash-keys of title->headline
                                             using (hash-values hash)
                                             append (list title hash)))
                     :-state->hash (append (org-glance-store--state->hash store) state->hash)
                     :-class->hash (append (org-glance-store--class->hash store) class->hash)
                     :wal (append store-wal wal))))

(cl-defun org-glance-store-remove-headlines (store &rest headlines)
  "Return `org-glance-store' with HEADLINES removed from STORE.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage. Watermark stays the same though.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-store-flush' method."
  (cl-loop
     with store-location = (org-glance-store-location store)
     with store-watermark = (org-glance-store-watermark store)
     with wal-location = (f-join store-location org-glance-store-wal-filename)
     with current-offset = (float-time)
     with title->hash = (org-glance-store--title->hash store)
     with state->hash = (org-glance-store--state->hash store)
     with class->hash = (org-glance-store--class->hash store)
     with removed-states = (make-hash-table)
     with removed-classes = (make-hash-table)
     for headline in headlines
     for event = (list current-offset 'RM headline)
     for title = (org-glance-headline-title headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     for hash = (org-glance-headline-hash headline)
     collect event into wal
     collect title into titles
     do (cl-loop for class in classes
           do (puthash (cons class hash) t removed-classes))
     do (puthash (cons state hash) t removed-states)
     finally do (org-glance-store-wal-append wal wal-location)
     finally return (org-glance-store--create
                     :location store-location
                     :watermark store-watermark
                     :-title->hash (apply #'a-dissoc title->hash titles)
                     :-state->hash (cl-loop for state in state->hash
                                      when (not (gethash state removed-states))
                                      collect state)
                     :-class->hash (cl-loop for class in class->hash
                                      when (not (gethash class removed-classes))
                                      collect class)
                     :wal (append (org-glance-store-wal store) wal))))

(cl-defun org-glance-store-wal-headlines (wal)
  "Return actual headlines from WAL."
  (cl-loop
     with seen = (make-hash-table :test #'equal)
     for (_ instruction headline) in (reverse wal)
     for hash = (org-glance-headline-hash headline)
     if (and (not (gethash hash seen)) (eq instruction 'PUT))
     collect headline
     else if (and (not (gethash hash seen)) (eq instruction 'RM))
     do (puthash hash t seen)))

(cl-defun org-glance-store-remove-headline-by-hash (store hash)
  "Return STORE with HASH removed."
  (if-let (headline (org-glance-store-get-headline-by-hash store hash))
      (org-glance-store-remove-headlines store headline)
    store))

(cl-defun org-glance-store-get-last-committed-offset-by-hash (store hash)
  "Return latest committed (cons offset headline) from STORE searched by HASH."
  (cl-loop for (offset instruction headline) in
       (reverse (org-glance-store-wal-read
                 (f-join (org-glance-store-location store)
                         org-glance-store-wal-filename)))
     if (string= hash (org-glance-headline-hash headline))
     return (cl-case instruction
              ('PUT (cons offset headline))
              ('RM (cons offset nil)))
     finally return (cons 0 nil)))

(cl-defun org-glance-store-get-headline-by-hash (store hash)
  "Return `org-glance-headline-header' from STORE searched by HASH."
  (cl-destructuring-bind (_ . headline)
      (org-glance-store-get-last-committed-offset-by-hash store hash)
    headline))

(cl-defun org-glance-store-get-headline-by-title (store title &optional (storage 'memory))
  "Return `org-glance-headline-header' from STORE searched by TITLE.

STORAGE specifies where to lookup: 'memory or 'disk."
  (cl-case storage
    ('disk (cl-loop
              with seen = (make-hash-table :test #'equal)
              with wal = (reverse (org-glance-store-wal store))
              for (_ _ headline) in wal
              for hash = (org-glance-headline-hash headline)
              for headline-location = (org-glance-store-headline-location store hash)
              for seen-p = (gethash hash seen)
              for title-matches-p = (string= title (org-glance-headline-title headline))
              for headline-exists-p = (f-exists-p headline-location)
              if (and title-matches-p headline-exists-p)
              return (org-glance-headline-load headline-location) ;;
              else if (not seen-p)
              do (puthash hash t seen)
              finally return nil))
    ('memory (a-get (org-glance-store--title->hash store) title))))

(cl-defun org-glance-store-wal-read (location)
  (when (and (f-exists-p location) (f-readable-p location))
    (with-temp-buffer
      (insert-file-contents-literally location)
      (goto-char (point-min))
      (cl-loop while (not (eobp))
         collect (read (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         do (forward-line)))))

(cl-defun org-glance-store-wal-append (wal location)
  (let ((contents (s-join "\n" (-map #'prin1-to-string wal))))
    (cond
      ((and (f-exists-p location) (f-empty-p location))
       (append-to-file contents nil location))
      ((f-exists-p location)
       (append-to-file (concat "\n" contents) nil location))
      (t (org-glance--with-temp-file location
           (insert contents))))))

(cl-defun org-glance-store-hashes (store)
  "Return actual headline hashes from STORE."
  (cl-loop
     with seen = (make-hash-table :test #'equal)
     for (_ instruction headline) in (reverse (org-glance-store-wal store))
     for hash = (org-glance-headline-hash headline)
     when (and (eq instruction 'PUT) (not (gethash hash seen)))
     collect (puthash hash hash seen)
     when (and (eq instruction 'RM) (not (gethash hash seen)))
     do (puthash hash hash seen)))

(cl-defgeneric org-glance-store-headline-location (store headline)
  "Return location of HEADLINE in STORE.")

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-headline-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline-header))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-headline-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-store-location store) "data" prefix postfix)))

(cl-defgeneric org-glance-store-headline (store headline)
  "Return fully qualified `org-glance-headline' from STORE using HEADLINE.")

(cl-defmethod org-glance-store-headline ((_ org-glance-store) (headline org-glance-headline))
  headline)

(cl-defmethod org-glance-store-headline ((store org-glance-store) (headline org-glance-headline-header))
  (org-glance-headline-load (org-glance-store-headline-location store headline)))

(cl-defmethod org-glance-store-headline ((store org-glance-store) (hash string))
  (org-glance-headline-load (org-glance-store-headline-location store hash)))

;; (cl-defun org-glance-store-completing-read (store)
;;   "Read headlines from STORE with completion."
;;   (let* ((titles (org-glance-store-i-title* store))
;;          (hash (alist-get (completing-read "Headline: " titles nil t) titles nil nil #'string=)))
;;     (org-glance-store-headline store hash)))

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (let ((headlines (-flatten (-map #'org-glance-file-headlines (org-glance-scope loc)))))
    (apply #'org-glance-store-put-headlines store headlines)))

(cl-defun org-glance-store-equal-p (a b)
  "Return t if A contains same headlines as B.

TODO should be optimized to not read all headlines from store."
  (let ((sorted-a (cl-loop for headline in (org-glance-store-hashes a)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-title it)
                                                     (org-glance-headline-title other))
                                            result)))
        (sorted-b (cl-loop for headline in (org-glance-store-hashes b)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-title it)
                                                     (org-glance-headline-title other)) result))))
    (and (not (null sorted-a))
         (not (null sorted-b))
         (--all? (and (consp it)
                      (org-glance-headline-equal-p (car it) (cdr it)))
                 (-zip sorted-a sorted-b)))))

(provide 'org-glance-store)
