;;; bench-material.el --- Material lifecycle measurements -*- lexical-binding: t; -*-

(require 'bench-common)

(defun org-glance-material-bench:run ()
  "Measure plain and crypt editor lifecycles and their family observations."
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
                 opened warm saved open-observations warm-observations save-observations)
            (apply #'org-glance-graph:add graph
                   (cl-loop for index below (1- size)
                            collect (org-glance-test:headline
                                     (format "other%d" index) "* Other")))
            (dotimes (_ depth) (org-glance-graph:add graph source))
            (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "pw"))
                      ((symbol-function 'org-glance-version:observe)
                       (lambda (&rest args) (cl-incf observations) (apply observe args))))
              (org-glance--discard-buffer (org-glance-material:open graph "bench" :decrypt crypt))
              (setq observations 0)
              (garbage-collect)
              (setq opened (benchmark-run 3
                             (setq buffer (org-glance-material:open graph "bench" :decrypt crypt))
                             (org-glance--discard-buffer buffer))
                    open-observations observations)
              (setq buffer (org-glance-material:open graph "bench" :decrypt crypt)
                    observations 0)
              (garbage-collect)
              (setq warm (benchmark-run 30 (org-glance-material:open graph "bench"))
                    warm-observations observations
                    observations 0)
              (with-current-buffer buffer
                (garbage-collect)
                (let ((gc-cons-threshold most-positive-fixnum))
                  (setq saved (benchmark-run 3
                                (goto-char (point-max))
                                (if crypt (search-backward "#+end_crypt") (insert "\n"))
                                (insert " edited\n")
                                (save-buffer)))))
              (setq save-observations observations)
              (org-glance--discard-buffer buffer))
            (let ((org-glance-bench--size size))
              (dolist (measurement
                       (list (list "editor-absent open" opened 3 open-observations)
                             (list "warm editor lookup" warm 30 warm-observations)
                             (list "save" saved 3 save-observations)))
                (let* ((run (nth 1 measurement))
                       (iterations (nth 2 measurement))
                       (milliseconds (* 1000 (/ (car run) iterations))))
                  (org-glance-bench:emit
                   "material" (format "%s %s depth=%d" (car measurement)
                                      (if crypt "crypt" "plain") depth)
                   (list :median-ms milliseconds :samples-ms (vector milliseconds)
                         :iterations iterations :gc-counts (vector (nth 1 run))
                         :gc-ms (vector (* 1000 (nth 2 run)))
                         :work (vector (list :family-observations (nth 3 measurement))))))))))))))

(provide 'bench-material)
;;; bench-material.el ends here
