;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-headline)

(cl-defstruct (org-glance-store (:constructor org-glance-store--create)
                                (:copier org-glance-store--copy))
  "Persistent store of headlines.
Implements indexes to optimize reads.
Builds and preserves indexes in actualized state."
  (location nil  :type string :read-only t :documentation "Directory where store persists.")
  (headlines nil :type list   :read-only t :documentation "List of headlines.")
  (origins (make-hash-table)  :read-only t :documentation "Headline to origin map."))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  (let ((origins (make-hash-table :test 'equal)))
    (org-glance-store--create
     :headlines (cl-loop for file in (org-glance-scope location)
                   append (cl-loop for headline in (org-glance-file-headlines file)
                             do (puthash (org-glance-headline-hash headline) file origins)
                             collect headline))
     :origins origins
     :location location)))

(cl-defun org-glance-store-headline-origin (store headline)
  (gethash (org-glance-headline-hash headline) (org-glance-store-origins store)))

(cl-defmethod org-glance-headlines ((store org-glance-store))
  "Retrieve headlines from STORE."
  (org-glance-store-headlines store))

(cl-defmethod org-glance-equal-p ((a org-glance-store) (b org-glance-store))
  "Return t if A contains same headlines as B."
  (let ((sorted-a (cl-loop for headline in (org-glance-headlines a)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-hash it) (org-glance-headline-hash other)) result)))
        (sorted-b (cl-loop for headline in (org-glance-headlines b)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-hash it) (org-glance-headline-hash other)) result))))
    (and (not (null sorted-a))
         (not (null sorted-b))
         (--all? (and (consp it)
                      (org-glance-equal-p (car it) (cdr it)))
                 (-zip sorted-a sorted-b)))))

(cl-defmethod org-glance-cardinality ((store org-glance-store))
  "Return number of headlines in STORE."
  (length (org-glance-headlines store)))

(cl-defmethod org-glance-materialize ((store org-glance-store) (file string))
  "Insert STORE into the FILE and provide ability to push changes
to its origins by calling `org-glance-commit'."
  (org-glance-with-temp-file file
    (insert "#    -*- mode: org; mode: org-glance-material -*-\n\n")
    (cl-loop for headline in (org-glance-headlines store)
       do (let ((origin (org-glance-store-headline-origin store headline)))
            (org-glance-headline-insert
             (-> headline
                 (org-glance-headline-set-org-properties "Hash" (org-glance-headline-hash headline))
                 (org-glance-headline-set-org-properties "Origin" origin)))))))

(cl-defmethod org-glance-save ((store org-glance-store) dest)
  "Save STORE to DEST."
  (when (f-exists? dest)
    (cond ((not (f-empty? dest)) (user-error "Destination exists and is not empty."))
          ((not (f-readable? dest)) (user-error "Destination exists and is not readable."))))

  (cl-flet ((save-index (index dest)
              (org-glance-with-temp-file dest
                (cl-loop for key being the hash-keys of index using (hash-values val)
                   do (cond ((null val) (insert key "\n"))
                            (t (insert key " " val "\n")))))))

    (let ((dimensions (list (a-list :name 'title    :map #'org-glance-headline-title)
                            (a-list :name 'archive  :filter #'org-glance-headline-archived-p)
                            (a-list :name 'comment  :filter #'org-glance-headline-commented-p)
                            (a-list :name 'closed   :filter #'org-glance-headline-closed-p)
                            (a-list :name 'crypt    :filter #'org-glance-headline-encrypted-p)
                            (a-list :name 'link     :filter #'org-glance-headline-linked-p)
                            (a-list :name 'property :filter #'org-glance-headline-propertized-p)))
          (index (make-hash-table)))

      (cl-loop for dimension in dimensions
         do (let-alist dimension
              (puthash .:name (make-hash-table :test 'equal) index)))

      (cl-loop for headline in (org-glance-headlines store)
         do
           (let* ((hash (org-glance-headline-hash headline))
                  (prefix (substring hash 0 2))
                  (postfix (substring hash 2 (length hash)))
                  (dest (f-join dest "data" prefix postfix)))

             (unless (file-exists-p dest)
               (org-glance-headline-export headline dest)
               (cl-loop for dimension in dimensions
                  do (let-alist dimension
                       (when (or (not .:filter) (funcall .:filter headline))
                         (puthash hash (when .:map (funcall .:map headline)) (gethash .:name index)))))))

         finally
           (cl-loop for dimension in dimensions
              do (let-alist dimension
                   (save-index (gethash .:name index) (f-join dest "index" (symbol-name .:name) "0"))))))))

(cl-defun org-glance-store-headline-index (location)
  (cl-flet* ((rs (beg end) (buffer-substring-no-properties beg end))
             (key () (rs (line-beginning-position) (+ (point) 32)))
             (val () (rs (+ (point) 32 1) (line-end-position))))
    (let ((sparse-index (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert-file-contents location)
        (goto-char (point-min))
        (cl-loop while (not (eobp))
           do (let* ((key (key))
                     (val (val))
                     (unique-val val)
                     (tryout 0))
                (while (gethash unique-val sparse-index)
                  (cl-incf tryout)
                  (setq unique-val (format "%s (%d)" val tryout)))
                (puthash unique-val key sparse-index))
             (forward-line)))
      sparse-index)))

;; (let ((store (org-glance-store "~/sync/views/song/resources")))
;;   (org-glance-save store "/tmp/songs"))

;; (let ((index (org-glance-store-headline-index "/tmp/songs/index/title/0")))
;;   (let* ((hash (gethash (completing-read "Headline: " index nil t) index))
;;          (prefix (substring hash 0 2))
;;          (postfix (substring hash 2 (length hash))))
;;     (org-glance-map-file (f-join "/tmp/songs" "data" prefix postfix)
;;       <headline>)))

(provide 'org-glance-store)
