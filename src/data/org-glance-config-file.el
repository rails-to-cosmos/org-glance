;;; org-glance-config-file.el --- Shared JSON file transactions -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(define-error 'org-glance-config-error "Invalid Glance configuration")
(define-error 'org-glance-config-busy "Configuration is locked" 'org-glance-config-error)
(define-error 'org-glance-config-conflict "Configuration changed" 'org-glance-config-error)
(define-error 'org-glance-config-unsafe "Unsafe configuration path" 'org-glance-config-error)

(cl-defstruct (org-glance-config-file
               (:constructor org-glance-config-file--make)
               (:conc-name org-glance-config-file:))
  revision object)

(defun org-glance-config-file:path (root)
  "Return ROOT's canonical configuration filename."
  (expand-file-name ".org-glance/config.json" root))

(defun org-glance-config-file--envelope (object)
  "Validate OBJECT's format envelope and return OBJECT."
  (unless (and (hash-table-p object)
               (numberp (gethash "format" object))
               (= 1 (gethash "format" object)))
    (signal 'org-glance-config-error '("config.json requires an object with format 1")))
  object)

(defun org-glance-config-file--decimal (text)
  "Return TEXT's exact decimal identity without floating-point conversion."
  (string-match
   "\\`\\(-?\\)\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?\\(?:[eE]\\([+-]?[0-9]+\\)\\)?\\'" text)
  (let* ((sign (match-string 1 text))
         (fraction (or (match-string 3 text) ""))
         (exponent (string-to-number (or (match-string 4 text) "0")))
         (digits (replace-regexp-in-string
                  "\\`0+" "" (concat (match-string 2 text) fraction)))
         (trimmed (replace-regexp-in-string "0+\\'" "" digits)))
    (if (string-empty-p digits) '("" 0)
      (list (concat sign trimmed)
            (+ exponent (- (length fraction)) (- (length digits) (length trimmed)))))))

(defun org-glance-config-file--numbers (text)
  "Refuse numeric literals in TEXT that native JSON would silently round."
  (let ((position 0) (size (length text)))
    (while (< position size)
      (let ((char (aref text position)))
        (cond
         ((= char ?\")
          (setq position (1+ position))
          (while (/= (aref text position) ?\")
            (setq position (+ position (if (= (aref text position) ?\\) 2 1))))
          (cl-incf position))
         ((or (= char ?-) (and (>= char ?0) (<= char ?9)))
          (string-match "-?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[eE][+-]?[0-9]+\\)?" text position)
          (let ((literal (match-string 0 text)))
            (setq position (match-end 0))
            (unless (equal (org-glance-config-file--decimal literal)
                           (org-glance-config-file--decimal
                            (json-serialize (json-parse-string literal))))
              (signal 'org-glance-config-error
                      '("config.json number cannot round-trip through both clients")))))
         (t (cl-incf position)))))))

(defun org-glance-config-file:decode (bytes)
  "Decode UTF-8 BYTES, rejecting duplicate keys and unsupported envelopes."
  (let ((text (decode-coding-string bytes 'utf-8-unix)))
    (when (cl-some (lambda (char) (> char #x10ffff)) text)
      (signal 'org-glance-config-error '("config.json is not valid UTF-8")))
    (cl-labels
        ((convert (value depth)
           (when (> depth 128)
             (signal 'org-glance-config-error '("config.json nesting exceeds 128 levels")))
           (cond
            ((listp value)
             (let ((table (make-hash-table :test 'equal)))
               (dolist (pair value)
                 (let ((key (symbol-name (car pair))))
                   (when (gethash key table)
                     (signal 'org-glance-config-error
                             (list (concat "duplicate config.json key: " key))))
                   (puthash key (cons t (convert (cdr pair) (1+ depth))) table)))
               (maphash (lambda (key item) (puthash key (cdr item) table)) table)
               table))
            ((vectorp value)
             (vconcat (mapcar (lambda (item) (convert item (1+ depth))) value)))
            (t value))))
      (let* ((parsed (condition-case err
                         (json-parse-string text :object-type 'alist
                                            :null-object :null :false-object :false)
                       (json-error
                        (signal 'org-glance-config-error (list (error-message-string err))))))
             (object (org-glance-config-file--envelope (convert parsed 0))))
        (org-glance-config-file--numbers text)
        (org-glance-config-file--make :revision (secure-hash 'sha256 bytes)
                                     :object object)))))

(defun org-glance-config-file--path (root create)
  "Check ROOT's file containment, creating its control directory with CREATE."
  (unless (file-directory-p root)
    (signal 'org-glance-config-unsafe (list root)))
  (let* ((directory (expand-file-name ".org-glance" (file-truename root)))
         (path (expand-file-name "config.json" directory)))
    (when (or (file-symlink-p directory)
              (and (file-exists-p directory) (not (file-directory-p directory))))
      (signal 'org-glance-config-unsafe (list directory)))
    (when (and create (not (file-directory-p directory)))
      (condition-case err
          (make-directory directory)
        (file-already-exists
         (unless (and (file-directory-p directory) (not (file-symlink-p directory)))
           (signal (car err) (cdr err))))))
    (when (or (file-symlink-p path)
              (and (file-exists-p path) (not (file-regular-p path))))
      (signal 'org-glance-config-unsafe (list path)))
    path))

(defun org-glance-config-file--read (path &optional observed)
  "Read PATH as one config snapshot; absence supplies the format envelope."
  (condition-case err
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path)
        (let ((bytes (buffer-string)))
          (if (and observed (org-glance-config-file:revision observed)
                   (equal (secure-hash 'sha256 bytes)
                          (org-glance-config-file:revision observed)))
              observed
            (org-glance-config-file:decode bytes))))
    (file-missing
     (if (file-exists-p path)
         (signal (car err) (cdr err))
       (let ((object (make-hash-table :test 'equal)))
         (puthash "format" 1 object)
         (org-glance-config-file--make :revision nil :object object))))))

(defun org-glance-config-file:read (root &optional observed)
  "Read ROOT's JSON configuration without creating files or acquiring a lock.
Reuse validated OBSERVED only when its exact byte revision still matches."
  (org-glance-config-file--read (org-glance-config-file--path root nil) observed))

(defun org-glance-config-file:with-lock (root action)
  "Run ACTION under ROOT's shared mkdir lock, refusing an existing owner."
  (let* ((path (org-glance-config-file--path root t))
         (lock (concat path ".lock"))
         (previous-inhibit-quit inhibit-quit)
         (inhibit-quit t))
    (condition-case nil
        (make-directory lock)
      (file-already-exists (signal 'org-glance-config-busy (list lock))))
    (unwind-protect
        (let ((inhibit-quit previous-inhibit-quit)) (funcall action))
      (delete-directory lock))))

(defun org-glance-config-file--serialize (object)
  "Encode OBJECT as UTF-8 JSON with a trailing newline."
  (let ((json (json-serialize object :null-object :null :false-object :false)))
    (concat (if (multibyte-string-p json)
                (encode-coding-string json 'utf-8-unix)
              json)
            "\n")))

(defun org-glance-config-file--publish (path bytes)
  "Replace PATH atomically with BYTES, cleaning up a failed temporary write."
  (let ((temporary (make-temp-file (concat path ".tmp."))))
    (unwind-protect
        (let ((coding-system-for-write 'no-conversion)
              (write-region-inhibit-fsync nil)
              (write-region-annotate-functions nil))
          (write-region bytes nil temporary nil 'silent)
          (when (file-exists-p path)
            (set-file-modes temporary (file-modes path)))
          (rename-file temporary path t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun org-glance-config-file--equal (left right)
  "Compare decoded JSON values LEFT and RIGHT independent of object order."
  (cond
   ((hash-table-p left)
    (and (hash-table-p right)
         (= (hash-table-count left) (hash-table-count right))
         (cl-loop for key being the hash-keys of left using (hash-values value)
                  always (org-glance-config-file--equal
                          value (gethash key right :missing)))))
   ((vectorp left)
    (and (vectorp right) (= (length left) (length right))
         (cl-loop for a across left for b across right
                  always (org-glance-config-file--equal a b))))
   ((and (numberp left) (numberp right))
    (or (equal left right)
        (equal (org-glance-config-file--decimal (json-serialize left))
               (org-glance-config-file--decimal (json-serialize right)))))
   (t (equal left right))))

(cl-defun org-glance-config-file:write (root &key expected change)
  "Apply CHANGE under ROOT's lock only when its revision equals EXPECTED.
CHANGE receives the complete object and must return a validated candidate."
  (org-glance-config-file:with-lock
   root
   (lambda ()
     (let* ((path (org-glance-config-file--path root nil))
            (current (org-glance-config-file--read path))
            (revision (org-glance-config-file:revision current)))
       (unless (equal revision expected)
         (signal 'org-glance-config-conflict (list expected revision)))
       (let* ((before (org-glance-config-file--serialize
                       (org-glance-config-file:object current)))
              (fields (org-glance-config-file--envelope
                       (funcall change (org-glance-config-file:object current))))
              (bytes (org-glance-config-file--serialize fields)))
         (if (and revision (equal before bytes))
             current
           (let ((candidate (org-glance-config-file:decode bytes))
                 (original (json-parse-string before
                            :null-object :null :false-object :false)))
             (if (and revision (org-glance-config-file--equal
                                original (org-glance-config-file:object candidate)))
                 (progn (setf (org-glance-config-file:object current) original) current)
               (org-glance-config-file--path root nil)
               (org-glance-config-file--publish path bytes)
               candidate))))))))

(provide 'org-glance-config-file)
;;; org-glance-config-file.el ends here
