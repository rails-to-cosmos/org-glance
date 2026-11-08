;;; bench-interactive.el --- Warm picker measurements -*- lexical-binding: t; -*-

(require 'org-glance)
(require 'benchmark)

(defun org-glance-interactive-bench--reference-candidates (metas filter)
  "Build METAS labels accepted by FILTER with the original linear lookups."
  (let* ((metas (cl-remove-if-not (or filter #'always) metas))
         (labels (mapcar #'org-glance-material:label metas))
         (counts (-frequencies labels)))
    (cl-mapcar
     (lambda (meta label)
       (cons (if (> (alist-get label counts 0 nil #'equal) 1)
                 (format "%s ·%s" label
                         (s-left 8 (org-glance-headline-metadata:id meta)))
               label)
             meta))
     metas labels)))

(defun org-glance-interactive-bench:run (directory &optional repeats)
  "Compare warm picker reads in DIRECTORY over REPEATS paired samples.
Read the committed cache through an inert graph handle; skip graph startup.
Print medians, decode counts and projection equality without headline text."
  (let* ((graph (make-org-glance-graph :directory (file-truename directory)))
         (inhibit-message t)
         (org-done-keywords (org-glance--done-keywords))
         (filter (org-glance-filter:predicate org-glance-filter-spec))
         (decode (symbol-function 'org-glance-cache--decode-metadata))
         (deserialize (symbol-function 'org-glance-headline-metadata:deserialize))
         (candidates (symbol-function 'org-glance-material--candidates))
         (baseline (make-hash-table))
         (optimized (make-hash-table))
         (decoded (vector 0 0))
         (fingerprints (vector nil nil))
         (rows 0))
    (org-glance-cache:headlines graph)
    (dotimes (sample (or repeats 7))
      (dolist (variant (if (zerop (% sample 2)) '(0 1) '(1 0)))
        (let ((table (if (zerop variant) baseline optimized)))
          (cl-letf (((symbol-function 'org-glance-cache--decode-metadata)
                     (if (zerop variant)
                         (lambda (payload)
                           (let ((org-glance-cache--decoded-records nil))
                             (funcall decode payload)))
                       decode))
                    ((symbol-function 'org-glance-headline-metadata:deserialize)
                     (lambda (record)
                       (cl-incf (aref decoded variant))
                       (funcall deserialize record)))
                    ((symbol-function 'org-glance-material--candidates)
                     (if (zerop variant)
                         #'org-glance-interactive-bench--reference-candidates
                       candidates)))
            (garbage-collect)
            (let* ((start (float-time))
                   (metas (org-glance-cache:headlines graph))
                   (read-time (- (float-time) start))
                   (start (float-time))
                   (labels (org-glance-material--candidates metas filter))
                   (label-time (- (float-time) start)))
              (setq rows (length metas))
              (push (* 1000 read-time) (gethash 'read table))
              (push (* 1000 label-time) (gethash 'labels table))
              (push (* 1000 (+ read-time label-time)) (gethash 'total table))
              (aset fingerprints variant (secure-hash 'sha256 (prin1-to-string labels))))))))
    (unless (equal (aref fingerprints 0) (aref fingerprints 1))
      (error "Warm picker projection changed"))
    (princ (format "Warm cache: %d rows, %d alternating pairs, equal projection\n"
                   rows (or repeats 7)))
    (dolist (key '(read labels total))
      (princ (format "%s median ms: baseline %.2f; optimized %.2f\n"
                     key
                     (nth (/ (length (gethash key baseline)) 2)
                          (sort (gethash key baseline) #'<))
                     (nth (/ (length (gethash key optimized)) 2)
                          (sort (gethash key optimized) #'<)))))
    (princ (format "Deserializations/read: baseline %.1f; optimized %.1f\n"
                   (/ (aref decoded 0) (float (or repeats 7)))
                   (/ (aref decoded 1) (float (or repeats 7)))))))

(provide 'bench-interactive)
;;; bench-interactive.el ends here
