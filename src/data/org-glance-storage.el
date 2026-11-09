;;; org-glance-storage.el --- Canonical storage format migrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Canonical format versions are independent of derived cache schemas.

;;; Code:

(require 'json)
(require 'org-glance-core)
(require 'org-glance-utils)

(define-error 'org-glance-storage-unsupported
  "Unsupported org-glance storage version" 'user-error)

(defconst org-glance-storage:version 1
  "Current canonical store layout version.")

(defconst org-glance-storage:formats
  '((store 1) (manifest 2) (headline-event 1 2) (wal 1))
  "Readable versions for each independent canonical format.")

(defvar org-glance-storage--records (make-hash-table :test #'equal)
  "Identity-guarded JSON format records, bounded to 128 paths.")

(defvar org-glance-storage--migration-lock nil
  "Canonical migration lock owned by the current call.")

(defun org-glance-storage--record-stamp (path)
  "Return PATH's identity and mutation stamp, bypassing symlinks."
  (when-let* ((attrs (file-attributes path)))
    (unless (stringp (car attrs))
      (list (file-attribute-size attrs) (file-attribute-modification-time attrs)
            (file-attribute-status-change-time attrs)
            (file-attribute-inode-number attrs) (file-attribute-device-number attrs)))))

(defun org-glance-storage--read-record (path)
  "Read PATH's JSON format record, reusing only an unchanged file identity."
  (let* ((stamp (org-glance-storage--record-stamp path))
         (held (gethash path org-glance-storage--records)))
    (if (and stamp held (equal stamp (car held))) (cdr held)
      (let ((record (with-temp-buffer
                      (insert-file-contents path)
                      (json-parse-buffer :object-type 'plist))))
        (when (and stamp (not (equal stamp (org-glance-storage--record-stamp path))))
          (error "Storage format record changed while reading %s" path))
        (when (and stamp (equal stamp (org-glance-storage--record-stamp path)))
          (when (>= (hash-table-count org-glance-storage--records) 128)
            (clrhash org-glance-storage--records))
          (puthash path (cons stamp record) org-glance-storage--records))
        record))))

(defun org-glance-storage:require-version (format version)
  "Return VERSION if FORMAT supports it; otherwise refuse access."
  (unless (memq version (cdr (assq format org-glance-storage:formats)))
    (signal 'org-glance-storage-unsupported
            (list (format "%s schema %S; supported versions: %S; upgrade org-glance"
                          format version (cdr (assq format org-glance-storage:formats))))))
  version)

(defun org-glance-storage:check (meta-directory)
  "Validate META-DIRECTORY's SCHEMA marker and return its layout version.
An absent marker denotes legacy layout zero; malformed markers refuse access."
  (let* ((path (expand-file-name "SCHEMA" meta-directory))
         (lock (concat path ".lock")))
    (when (and (file-exists-p lock) (not (equal lock org-glance-storage--migration-lock)))
      (user-error "Canonical storage migration is locked at %s" lock))
    (if (not (file-exists-p path)) 0
      (condition-case err
          (let ((record (org-glance-storage--read-record path)))
            (dolist (entry '((:version . store) (:manifest . manifest)
                             (:headlineEvent . headline-event) (:wal . wal)))
              (org-glance-storage:require-version
               (cdr entry) (plist-get record (car entry))))
            (plist-get record :version))
        (org-glance-storage-unsupported (signal (car err) (cdr err)))
        (error (user-error "Cannot read canonical storage schema at %s: %s"
                           path (error-message-string err)))))))

(defun org-glance-storage--adopt-legacy (meta-directory)
  "Validate META-DIRECTORY's legacy MANIFEST before adopting its layout."
  (let ((record (org-glance-storage--read-record
                 (expand-file-name "MANIFEST" meta-directory))))
    (org-glance-storage:require-version 'manifest (or (plist-get record :version) 2))
    (unless (vectorp (plist-get record :segments))
      (error "Canonical storage migration requires a recovered MANIFEST"))))

(defconst org-glance-storage--migrations
  '((0 1 org-glance-storage--adopt-legacy))
  "Ordered (FROM TO FUNCTION) canonical layout migration steps.")

(defun org-glance-storage--migrate-locked (meta-directory)
  "Run META-DIRECTORY's migration steps while holding its canonical lock."
  (let ((version (org-glance-storage:check meta-directory)))
    (while (< version org-glance-storage:version)
      (let* ((step (assq version org-glance-storage--migrations))
             (next (cadr step)))
        (unless (and step (= next (1+ version)))
          (error "Missing canonical storage migration from %s" version))
        (funcall (nth 2 step) meta-directory)
        (org-glance--atomic-write
         (expand-file-name "SCHEMA" meta-directory)
         (concat (json-serialize
                  (list :version next :manifest 2 :headlineEvent 2 :wal 1)) "\n"))
        (setq version next)))
    version))

(defun org-glance-storage:migrate (meta-directory)
  "Apply known migrations in META-DIRECTORY under its shared mkdir lock.
Commit versions atomically after success; repeating a completed step is inert."
  (let ((version (org-glance-storage:check meta-directory)))
    (if (= version org-glance-storage:version) version
      (let ((org-glance-storage--migration-lock
             (expand-file-name "SCHEMA.lock" meta-directory)))
        (condition-case nil
            (make-directory org-glance-storage--migration-lock)
          (file-already-exists
           (user-error "Canonical storage migration is locked at %s"
                       org-glance-storage--migration-lock)))
        (unwind-protect
            (org-glance-storage--migrate-locked meta-directory)
          (delete-directory org-glance-storage--migration-lock))))))

(provide 'org-glance-storage)
;;; org-glance-storage.el ends here
