(require 'org-glance-headline)

;; (defconst org-glance-index-dimensions
;;   (list (a-list :name 'title    :map #'org-glance-headline-title)
;;         ;; (a-list :name 'archive  :filter #'org-glance-headline-archived-p)
;;         ;; (a-list :name 'comment  :filter #'org-glance-headline-commented-p)
;;         ;; (a-list :name 'closed   :filter #'org-glance-headline-closed-p)
;;         ;; (a-list :name 'crypt    :filter #'org-glance-headline-encrypted-p)
;;         ;; (a-list :name 'link     :filter #'org-glance-headline-linked-p)
;;         ;; (a-list :name 'property :filter #'org-glance-headline-propertized-p)
;;         ))

(cl-defstruct (org-glance-index (:constructor org-glance-index--create)
                                (:copier nil))
  (location nil :type string :read-only t)
  (memtbl-a nil :type list :read-only t)
  (memtbl-b nil :type list :read-only nil)
  (treshold 100 :type number :read-only t))

;; GET: query B, query A, query SST location
;; SET: write "SET" instruction to B
;; RM: write "RM" instruction to B
;; data field -- is store hash/id

(cl-defun org-glance-index (headlines &key map filter)
  "Build index of HEADLINES."
  ;; mappers and filters should be generics and work properly with full and dummy headlines
  (cl-loop for headline in (seq-uniq headlines #'org-glance-headline-equal-p)
     when (or (not filter) (funcall filter headline))
     collect (cons (org-glance-headline-hash headline)
                   (when map (funcall map headline)))))

(cl-defun org-glance-index-read (location)
  "Read index from LOC."
  (when (and (f-exists-p location) (f-readable-p location))
    (let ((part-0 (f-join location "0")))
      (with-temp-buffer
        (insert-file-contents part-0)
        (goto-char (point-min))
        (cl-loop while (not (eobp))
           when (> (- (line-end-position) (line-beginning-position)) 32)
           collect (cons (buffer-substring-no-properties (line-beginning-position) (+ (point) 32))
                         (buffer-substring-no-properties (+ (point) 32 1) (line-end-position)))
           into index
           do (forward-line)
           finally return (seq-uniq index (lambda (a b) (string= (car a) (car b)))))))))

(cl-defun org-glance-index-serialize (idx)
  "Serialize index IDX."
  (--map (cond ((null (cdr it)) (car it))
               (t (concat (car it) " " (cdr it))))
         idx))

(cl-defun org-glance-index-write (idx location)
  "Write index IDX to LOCATION."
  (org-glance--with-temp-file location
    (insert (s-join "\n" (org-glance-index-serialize idx)))))

(cl-defun org-glance-index-append (idx location)
  "Append index IDX to LOCATION."
  (mkdir location t)
  (let ((part-0 (f-join location "0")))
    (cond ((and (f-exists-p part-0) (f-empty-p part-0))
           (append-to-file (s-join "\n" (org-glance-index-serialize idx)) nil part-0))
          ((f-exists-p part-0)
           (append-to-file (concat "\n" (s-join "\n" (org-glance-index-serialize idx))) nil part-0))
          (t (org-glance-index-write idx part-0)))))

(cl-defun org-glance-index-inversed (idx)
  "Reverse index IDX."
  (--map (cons (cdr it) (car it)) idx))

(cl-defun org-glance-index-uniq (idx &optional (test #'equal))
  (cl-loop with set = (make-hash-table :test test)
     for (key . val) in idx
     collect (let ((try 0)
                   (ukey key))
               (while (gethash ukey set)
                 (cl-incf try)
                 (setq ukey (format "%s (%d)" key try)))
               (puthash ukey t set)
               (cons ukey val))))

(cl-defun org-glance-index-merge (&rest idxs)
  (apply #'a-merge idxs))

;; TODO uniquify on write?
;; (let* ((kv (org-glance-store--parse-index-line))
;;        (key (car kv))
;;        (val (cdr kv))
;;        (unique-val val)
;;        (tryout 0))
;;   (while (gethash unique-val result)
;;     (cl-incf tryout)
;;     (setq unique-val (format "%s (%d)" val tryout)))
;;   (prog1 (list unique-val key)
;;     (forward-line)))


(provide 'org-glance-index)
