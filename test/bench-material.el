;;; bench-material.el --- Material lifecycle measurements -*- lexical-binding: t; -*-

(require 'bench-common)
(require 'benchmark)

(defun org-glance-material-bench:run ()
  "Measure open, warm lookup and save latency over plain and crypt families.
Print milliseconds per operation and family observations per save."
  (let ((inhibit-message t) (gc-cons-threshold (* 128 1024 1024)))
    (dolist (shape (if (getenv "OG_BENCH_QUICK") '((10 1))
                    (append '((10 1) (50 12) (100 40))
                            (when (getenv "OG_BENCH_LARGE") '((500 80))))))
      (dolist (crypt '(nil t))
        (org-glance-test:with-graph graph
          (let* ((size (car shape)) (depth (cadr shape))
                 (headline (org-glance-test:headline "bench" "* TODO Bench" "body"))
                 (source (if crypt (org-glance-headline:encrypt headline "pw") headline))
                 (buffer nil) (observations 0)
                 (observe (symbol-function 'org-glance-version:observe))
                 opened warm saved save-observations)
            (apply #'org-glance-graph:add graph
                   (cl-loop for index below (1- size)
                            collect (org-glance-test:headline
                                     (format "other%d" index) "* Other")))
            (dotimes (_ depth) (org-glance-graph:add graph source))
            (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "pw")))
              (org-glance--discard-buffer (org-glance-material:open graph "bench" :decrypt crypt))
              (garbage-collect)
              (setq opened
                    (car (benchmark-run 3
                           (setq buffer (org-glance-material:open graph "bench" :decrypt crypt))
                           (org-glance--discard-buffer buffer))))
              (setq buffer (org-glance-material:open graph "bench" :decrypt crypt))
              (cl-letf (((symbol-function 'org-glance-version:observe)
                         (lambda (&rest args) (cl-incf observations) (apply observe args))))
                (garbage-collect)
                (setq warm (car (benchmark-run 30 (org-glance-material:open graph "bench"))))
                (setq observations 0)
                (with-current-buffer buffer
                  (garbage-collect)
                  (let ((gc-cons-threshold most-positive-fixnum))
                    (setq saved
                          (car (benchmark-run 3
                                 (goto-char (point-max))
                                 (if crypt (search-backward "#+end_crypt") (insert "\n"))
                                 (insert " edited\n")
                                 (save-buffer))))))
                (setq save-observations observations))
              (org-glance--discard-buffer buffer))
            (let ((org-glance-bench--size size))
              (dolist (measurement (list (list "editor-absent open" (/ opened 3))
                                        (list "warm editor lookup" (/ warm 30))
                                        (list "save" (/ saved 3))))
                (org-glance-bench:emit
                 "material" (format "%s %s depth=%d" (car measurement)
                                    (if crypt "crypt" "plain") depth)
                 (list :median-ms (* 1000 (cadr measurement))
                       :samples-ms (vector (* 1000 (cadr measurement)))
                       :work (vector (list :observations-per-save
                                           (/ save-observations 3.0)))))))))))))


(provide 'bench-material)
