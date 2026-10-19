;;; overview-export-peer.el --- Concurrent export peer -*- lexical-binding: t -*-
(unless (fboundp 'org-glance-test:org-with-id)
  (load (expand-file-name "../test-helpers.el" (file-name-directory load-file-name)) nil t))
(unless (fboundp 'org-glance-config-test--publish)
  (load (expand-file-name "../test-config.el" (file-name-directory load-file-name)) nil t))

(defun org-glance-test-overview-wait (root name)
  "Wait for scheduling barrier NAME in disposable ROOT."
  (let ((deadline (+ (float-time) 15)))
    (while (not (file-exists-p (expand-file-name name root)))
      (when (> (float-time) deadline) (error "Export barrier timed out: %s" name))
      (accept-process-output nil 0.01))))

(defun org-glance-test-overview-signal (root name)
  "Publish scheduling barrier NAME in disposable ROOT."
  (org-glance--atomic-write (expand-file-name name root) "ready"))

(defun org-glance-test-overview-change (graph kind)
  "Publish a real presentation or family-only change KIND to GRAPH."
  (if (eq kind 'presentation)
      (org-glance-config-test--publish graph
       "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(q)\"]}]}")
    (let* ((id "export-row") (family (org-glance-graph:headline-data-path graph id)))
      (org-glance-version:write-snapshot family id
       (mapcar #'org-glance-version:id
               (org-glance-version:leaves (org-glance-version:history family id)))
       "test" (org-glance-test:org-with-id "* PLAN Updated :book:" id) 10)
      (org-glance-cache:reconcile-inventory graph t))))

(defun org-glance-test-overview-peer (root role filter kind serial)
  "Run one real export publisher ROLE in ROOT with FILTER, KIND and SERIAL."
  (when-let* ((frozen (getenv "GLANCE_OVERVIEW_FROZEN"))) (load frozen nil t))
  (let* ((graph (org-glance-graph root))
         (write (symbol-function 'org-glance--atomic-write))
         (expected (if (eq kind 'presentation) "PLAN(q)" "Updated")))
    (when (timerp org-glance-cache--inventory-timer)
      (cancel-timer org-glance-cache--inventory-timer))
    (if (eq role 'old)
        (unwind-protect
            (condition-case error
                (cl-letf (((symbol-function 'org-glance--atomic-write)
                           (lambda (path text &rest arguments)
                             (when (string-suffix-p ".org" path)
                               (org-glance-test-overview-signal root "old-prepared")
                               (unless serial (org-glance-test-overview-wait root "new-artifact")))
                             (apply write path text arguments))))
                  (org-glance-overview:write graph filter))
              (user-error
               (unless (string-prefix-p "Overview inputs changed" (error-message-string error))
                 (signal (car error) (cdr error)))
               (princ (format "OLD_REFUSED %s\n" (error-message-string error)))))
          (org-glance-test-overview-signal root "old-finished"))
      (org-glance-test-overview-change graph kind)
      (let ((reader-checked nil))
        (cl-letf (((symbol-function 'org-glance--atomic-write)
                   (lambda (path text &rest arguments)
                     (prog1 (apply write path text arguments)
                       (when (and (string-suffix-p ".org" path) (not reader-checked))
                         (setq reader-checked t)
                         (org-glance-test-overview-signal root "new-artifact")
                         (unless serial (org-glance-test-overview-wait root "old-finished"))
                         (cl-letf (((symbol-function 'org-glance-cache:overview-rows)
                                    (lambda (&rest _) (error "Reader requires refresh"))))
                           (condition-case error
                               (let ((file (org-glance-overview:cached-file graph filter)))
                                 (unless (string-search expected (f-read-text file 'utf-8))
                                   (error "Reader between publications admitted stale bytes"))
                                 (princ "READER_HIT\n"))
                             (error
                              (unless (equal "Reader requires refresh" (error-message-string error))
                                (signal (car error) (cdr error)))
                              (princ "READER_MISS\n")))))))))
          (org-glance-overview:write graph filter)))
      (cl-letf (((symbol-function 'org-glance-cache:overview-rows)
                 (lambda (&rest _) (error "Final warm admission fetched Rows"))))
        (let ((file (org-glance-overview:cached-file graph filter)))
          (unless (and (org-glance-overview--spec-owns-cache? filter file graph)
                       (string-search expected (f-read-text file 'utf-8)))
            (error "Final warm admission certified stale bytes"))
          (princ (format "EXPORT_OK %S %S %S %s\n" filter kind serial file)))))))

(defun org-glance-test-overview-interrupt-peer (root filter kind)
  "Pause after publishing a real artifact in ROOT, before its FILTER manifest."
  (let* ((graph (org-glance-graph root))
         (write (symbol-function 'org-glance--atomic-write)))
    (org-glance-test-overview-change graph kind)
    (cl-letf (((symbol-function 'org-glance--atomic-write)
               (lambda (path text &rest arguments)
                 (prog1 (apply write path text arguments)
                   (when (string-suffix-p ".org" path)
                     (org-glance-test-overview-signal root "publisher-paused")
                     (org-glance-test-overview-wait root "never-released"))))))
      (org-glance-overview:write graph filter))))
