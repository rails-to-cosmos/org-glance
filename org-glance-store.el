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

  ;; lru cache
  (-hash->headline (a-list) :type list :read-only t)

  ;; indexes
  (-title->hash (a-list) :type list :read-only t)
  (-state->hash (a-list) :type list :read-only t)
  (-class->hash (a-list) :type list :read-only t)
  (-archived->hash (a-list) :type list :read-only t)
  (-commented->hash (a-list) :type list :read-only t)
  (-closed->hash (a-list) :type list :read-only t)
  (-linked->hash (a-list) :type list :read-only t)
  (-propertized->hash (a-list) :type list :read-only t)
  (-encrypted->hash (a-list) :type list :read-only t)
  ;; (ts->headline (a-list) :type list :read-only t) ;; interval tree

  (wal nil :type list :read-only t :documentation "Append-only event log."))

(cl-defun org-glance-store (location)
  "Create or read store binded to directory LOCATION."
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
  (cl-loop
     with store = (org-glance-store location)
     for string in strings
     for headline = (org-glance-headline-from-string string)
     collect headline into headlines
     finally do (org-glance-store-put-headlines store headlines)
     finally return (org-glance-store-read location)))

(cl-defun org-glance-store/ (store location)
  (apply #'f-join (org-glance-store-location store) (s-split "/" location)))

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
                  (with-temp-file (org-glance-store/ store org-glance-store-watermark-filename)
                    (insert (prin1-to-string offset))))
     finally return store))

(cl-defun org-glance-store-read (location)
  "Read `org-glance-store' from LOCATION."
  (cl-loop
     with wal = (org-glance-store-wal-read (f-join location org-glance-store-wal-filename))
     with headlines = (org-glance-store-wal-headlines wal)
     with title->hash = (make-hash-table :test #'equal)
     for headline in headlines
     for hash = (org-glance-headline-hash headline)
     for title = (org-glance-headline-title headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     for commented = (org-glance-headline-commented-p headline)
     for archived = (org-glance-headline-archived-p headline)
     for closed = (org-glance-headline-closed-p headline)
     for linked = (org-glance-headline-linked-p headline)
     for propertized = (org-glance-headline-propertized-p headline)
     for encrypted = (org-glance-headline-encrypted-p headline)
     do (org-glance-store--uniquify title hash title->hash)
     collect (cons hash headline) into hash->headline
     collect (cons state hash) into state->hash
     collect (cons commented hash) into commented->hash
     collect (cons archived hash) into archived->hash
     collect (cons closed hash) into closed->hash
     collect (cons linked hash) into linked->hash
     collect (cons propertized hash) into propertized->hash
     collect (cons encrypted hash) into encrypted->hash
     append (or (--map (cons it hash) classes) (list (cons nil hash))) into class->hash
     finally return
       (org-glance-store--create
        :location location
        :watermark (cond ((f-exists-p (f-join location org-glance-store-watermark-filename))
                          (with-temp-buffer
                            (insert-file-contents-literally (f-join location org-glance-store-watermark-filename))
                            (read (buffer-substring (point-min) (point-max)))))
                         ((null wal) (float-time))
                         (t (cl-destructuring-bind (offset _ _) (car (last wal))
                              offset)))
        :-hash->headline hash->headline
        :-title->hash (cl-loop for title being the hash-keys of title->hash
                         using (hash-values hash)
                         collect (cons title hash))
        :-state->hash state->hash
        :-class->hash class->hash
        :-commented->hash commented->hash
        :-archived->hash archived->hash
        :-closed->hash closed->hash
        :-linked->hash linked->hash
        :-propertized->hash propertized->hash
        :-encrypted->hash encrypted->hash
        :wal wal)))

(cl-defun org-glance-store-filter (store filter)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES filtered by FILTER."
  (cl-loop
     with current-offset = (float-time)
     with title->hash = (make-hash-table :test #'equal)
     with store-location = (org-glance-store-location store)
     with store-watermark = (org-glance-store-watermark store)
     for hash in (org-glance-store-hashes store)
     for headline = (org-glance-store-headline store hash)
     for header = (org-glance-headline-header headline)
     for title = (org-glance-headline-title headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     for commented = (org-glance-headline-commented-p headline)
     for archived = (org-glance-headline-archived-p headline)
     for closed = (org-glance-headline-closed-p headline)
     for linked = (org-glance-headline-linked-p headline)
     for propertized = (org-glance-headline-propertized-p headline)
     for encrypted = (org-glance-headline-encrypted-p headline)
     for event = (list current-offset 'PUT header)
     for filtered-p = (or (null filter) (funcall filter headline))

     when filtered-p
     do (org-glance-store--uniquify title hash title->hash)
     when filtered-p
     collect (cons hash headline) into hash->headline
     when filtered-p
     collect (cons state hash) into state->hash
     when filtered-p
     collect (cons commented hash) into commented->hash
     when filtered-p
     collect (cons archived hash) into archived->hash
     when filtered-p
     collect (cons closed hash) into closed->hash
     when filtered-p
     collect (cons linked hash) into linked->hash
     when filtered-p
     collect (cons propertized hash) into propertized->hash
     when filtered-p
     collect (cons encrypted hash) into encrypted->hash
     when filtered-p
     append (or (--map (cons it hash) classes) (list (cons nil hash))) into class->hash
     when filtered-p
     collect event into wal

     finally return
       (org-glance-store--create
        :location store-location
        :watermark store-watermark
        :-hash->headline hash->headline
        :-title->hash (cl-loop for title being the hash-keys of title->hash
                         using (hash-values hash)
                         collect (cons title hash))
        :-state->hash state->hash
        :-class->hash class->hash
        :-commented->hash commented->hash
        :-archived->hash archived->hash
        :-closed->hash closed->hash
        :-linked->hash linked->hash
        :-propertized->hash propertized->hash
        :-encrypted->hash encrypted->hash
        :wal wal)))

(cl-defun org-glance-store-put-headlines (store headlines)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it.

Append PUT event to WAL and insert headlines to persistent storage."
  (cl-loop
     with current-offset = (float-time)
     with title->hash = (make-hash-table :test #'equal)
     with store-location = (org-glance-store-location store)
     with store-watermark = (org-glance-store-watermark store)
     with store-wal = (org-glance-store-wal store)
     for headline in headlines
     for title = (org-glance-headline-title headline)
     for hash = (org-glance-headline-hash headline)
     for state = (org-glance-headline-state headline)
     for classes = (org-glance-headline-class headline)
     for commented = (org-glance-headline-commented-p headline)
     for archived = (org-glance-headline-archived-p headline)
     for closed = (org-glance-headline-closed-p headline)
     for linked = (org-glance-headline-linked-p headline)
     for propertized = (org-glance-headline-propertized-p headline)
     for encrypted = (org-glance-headline-encrypted-p headline)
     for location = (org-glance-store-headline-location store headline)
     for event = (list current-offset 'PUT (org-glance-headline-header headline))
     unless (f-exists-p location) ;; could be made in a separate thread
     ;; no need to write fully qualified headlines, write only headers
     do (org-glance-headline-save headline location)
     do (org-glance-store--uniquify title hash title->hash)
     collect (cons hash headline) into hash->headline
     collect (cons state hash) into state->hash
     collect (cons commented hash) into commented->hash
     collect (cons archived hash) into archived->hash
     collect (cons closed hash) into closed->hash
     collect (cons linked hash) into linked->hash
     collect (cons propertized hash) into propertized->hash
     collect (cons encrypted hash) into encrypted->hash
     append (or (--map (cons it hash) classes) (list (cons nil hash))) into class->hash
     collect event into wal
     ;; append to wal in persistent storage
     finally do (org-glance-store-wal-append wal (f-join (org-glance-store-location store) org-glance-store-wal-filename))
     ;; return in-memory wal representation
     finally return
       (org-glance-store--create
        :location store-location
        :watermark store-watermark
        :-hash->headline (append hash->headline (org-glance-store--hash->headline store))
        :-title->hash (apply #'a-assoc
                             (org-glance-store--title->hash store)
                             (cl-loop for title being the hash-keys of title->hash
                                using (hash-values hash)
                                append (list title hash)))
        :-state->hash (append (org-glance-store--state->hash store) state->hash)
        :-class->hash (append (org-glance-store--class->hash store) class->hash)
        :-commented->hash (append (org-glance-store--commented->hash store) commented->hash)
        :-archived->hash (append (org-glance-store--archived->hash store) archived->hash)
        :-closed->hash (append (org-glance-store--closed->hash store) closed->hash)
        :-linked->hash (append (org-glance-store--linked->hash store) linked->hash)
        :-propertized->hash (append (org-glance-store--propertized->hash store) propertized->hash)
        :-encrypted->hash (append (org-glance-store--encrypted->hash store) encrypted->hash)
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
     with hash->headline = (org-glance-store--hash->headline store)
     with title->hash = (org-glance-store--title->hash store)
     with state->hash = (org-glance-store--state->hash store)
     with class->hash = (org-glance-store--class->hash store)
     with commented->hash = (org-glance-store--commented->hash store)
     with archived->hash = (org-glance-store--archived->hash store)
     with closed->hash = (org-glance-store--closed->hash store)
     with linked->hash = (org-glance-store--linked->hash store)
     with propertized->hash = (org-glance-store--propertized->hash store)
     with encrypted->hash = (org-glance-store--encrypted->hash store)
     with removed-states = (make-hash-table)
     with removed-classes = (make-hash-table)
     with removed-commented = (make-hash-table)
     with removed-archived = (make-hash-table)
     with removed-closed = (make-hash-table)
     with removed-linked = (make-hash-table)
     with removed-propertized = (make-hash-table)
     with removed-encrypted = (make-hash-table)
     for headline in headlines
     for event = (list current-offset 'RM headline)
     for title = (org-glance-headline-title headline)
     for state = (org-glance-headline-state headline)
     for commented = (org-glance-headline-commented-p headline)
     for archived = (org-glance-headline-archived-p headline)
     for closed = (org-glance-headline-closed-p headline)
     for linked = (org-glance-headline-linked-p headline)
     for propertized = (org-glance-headline-propertized-p headline)
     for encrypted = (org-glance-headline-encrypted-p headline)
     for classes = (org-glance-headline-class headline)
     for hash = (org-glance-headline-hash headline)
     collect hash into hashes
     collect event into wal
     collect title into titles
     do (cl-loop for class in classes
           do (puthash (cons class hash) t removed-classes))
     do (puthash (cons state hash) t removed-states)
     do (puthash (cons commented hash) t removed-commented)
     do (puthash (cons archived hash) t removed-archived)
     do (puthash (cons closed hash) t removed-closed)
     do (puthash (cons linked hash) t removed-linked)
     do (puthash (cons propertized hash) t removed-propertized)
     do (puthash (cons encrypted hash) t removed-encrypted)
     finally do (org-glance-store-wal-append wal wal-location)
     finally return
       (org-glance-store--create
        :location store-location
        :watermark store-watermark
        :-hash->headline (apply #'a-dissoc hash->headline hashes)
        :-title->hash (apply #'a-dissoc title->hash titles)
        :-state->hash (cl-loop for state in state->hash
                         when (not (gethash state removed-states))
                         collect state)
        :-class->hash (cl-loop for class in class->hash
                         when (not (gethash class removed-classes))
                         collect class)
        :-commented->hash (cl-loop for commented in commented->hash
                             when (not (gethash commented removed-commented))
                             collect commented)
        :-archived->hash (cl-loop for archived in archived->hash
                            when (not (gethash archived removed-archived))
                            collect archived)
        :-closed->hash (cl-loop for closed in closed->hash
                          when (not (gethash closed removed-closed))
                          collect closed)
        :-linked->hash (cl-loop for linked in linked->hash
                          when (not (gethash linked removed-linked))
                          collect linked)
        :-propertized->hash (cl-loop for propertized in propertized->hash
                               when (not (gethash propertized removed-propertized))
                               collect propertized)
        :-encrypted->hash (cl-loop for encrypted in encrypted->hash
                             when (not (gethash encrypted removed-encrypted))
                             collect encrypted)
        :wal (append (org-glance-store-wal store) wal))))

(cl-defun org-glance-store--uniquify (key value hash-table)
  (let ((uniq-key key)
        (tryout 1))
    (while (gethash uniq-key hash-table)
      (cl-incf tryout)
      (setq uniq-key (format "%s (%d)" key tryout)))
    (puthash uniq-key value hash-table)))

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
  (cl-loop
     for (offset instruction headline) in (reverse (org-glance-store-wal-read
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
              with wal = (reverse (org-glance-store-wal-read
                                   (org-glance-store/ store org-glance-store-wal-filename)))
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
    (cond ((and (f-exists-p location) (f-empty-p location))
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

(cl-defun org-glance-store-headlines (store)
  "Return actual headline hashes from STORE."
  (cl-loop for hash in (org-glance-store-hashes store)
     collect (org-glance-store-headline store hash)))

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

(cl-defun org-glance-store-completing-read (store)
  "Read headlines from STORE with completion."
  (let* ((title->hash (org-glance-store--title->hash store))
         (hash (alist-get (completing-read "Headline: " title->hash nil t) title->hash nil nil #'string=)))
    (org-glance-store-headline store hash)))

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (cl-loop for file in (org-glance-scope loc)
     append (org-glance--with-temp-buffer
             (insert-file-contents file)
             (org-glance-map (headline) headline))
     into headlines
     finally return (org-glance-store-put-headlines store headlines)))

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
