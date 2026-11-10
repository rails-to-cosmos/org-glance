;;; org-glance-perf.el --- Interactive latency measurements -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `org-glance-perf-mode' to report command stages in *Messages*.

;;; Code:

(require 'cl-lib)
(require 'org-glance-core)

(defvar org-glance-perf--trace nil)
(defvar org-glance-perf--serial 0)
(defvar org-glance-perf--origin nil)
(defvar org-glance-perf--advices nil)
(defvar org-glance-perf--timers nil)
(defvar org-glance-perf-mode nil)
(defvar org-glance-perf--current nil)
(defvar org-glance-perf--depth 0)
(defvar org-glance-perf--reporting 0.0)

(defun org-glance-perf--clear-advices ()
  "Remove every advice installed by the timing mode."
  (dolist (entry org-glance-perf--advices)
    (advice-remove (car entry) (cdr entry)))
  (setq org-glance-perf--advices nil))

(defun org-glance-perf--set-functions (symbol value)
  "Set timing option SYMBOL to VALUE and refresh enabled instrumentation."
  (set-default symbol (delete-dups (copy-sequence value)))
  (when (and org-glance-perf-mode (fboundp 'org-glance-perf--install)
             (boundp 'org-glance-perf-commands) (boundp 'org-glance-perf-stages)
             (boundp 'org-glance-perf-continuations))
    (org-glance-perf--clear-advices)
    (org-glance-perf--install)))

(defcustom org-glance-perf-commands
  '(org-glance-transient org-glance-materialize org-glance-overview
    org-glance-table org-glance-tags org-glance-open)
  "Function symbols whose synchronous UX preparation is measured."
  :type '(repeat symbol) :group 'org-glance
  :set #'org-glance-perf--set-functions)

(defcustom org-glance-perf-stages
  '(org-glance-ensure-init org-glance-material:pick-metadata
    org-glance-material:completing-read org-glance-material--selection
    org-glance-cache:headlines org-glance-cache--read-db
    org-glance-cache--acquire-reader org-glance-cache--call-reader
    org-glance-cache--ordinary-rows org-glance-cache--read-projection
    org-glance-cache--decode-metadata org-glance-material--candidates
    org-glance-config:cache-key org-glance-material:open
    org-glance-cache:logical-id org-glance-cache:material-state
    org-glance-material--create org-glance-material--load-source
    org-glance-material--read-source org-glance-cache:incoming-relations
    org-glance-version:observe org-glance-property-index:property
    transient-setup org-mode switch-to-buffer pop-to-buffer read-from-minibuffer)
  "Function symbols aggregated by call count and inclusive elapsed time."
  :type '(repeat symbol) :group 'org-glance
  :set #'org-glance-perf--set-functions)

(defcustom org-glance-perf-continuations '(org-glance-transient)
  "Command symbols that retain the transient origin for a following step."
  :type '(repeat symbol) :group 'org-glance
  :set #'org-glance-perf--set-functions)

(defcustom org-glance-perf-live nil
  "Non-nil means `org-glance-perf-mode' reports each stage as it returns.
Live lines are indented by nesting depth; their output time is excluded
from enclosing stages and the command total."
  :type 'boolean :group 'org-glance)

(cl-defun org-glance-perf:register (&key commands stages continuations)
  "Append COMMANDS, STAGES and CONTINUATIONS to the timing configuration.
Accept function symbols before their libraries load; repeated entries collapse."
  (dolist (entries (list commands stages continuations))
    (unless (and (listp entries) (cl-every #'symbolp entries))
      (error "Timing registration requires lists of function symbols")))
  (setq org-glance-perf-commands (delete-dups (append org-glance-perf-commands commands))
        org-glance-perf-stages (delete-dups (append org-glance-perf-stages stages))
        org-glance-perf-continuations
        (delete-dups (append org-glance-perf-continuations continuations)))
  (when org-glance-perf-mode
    (org-glance-perf--clear-advices)
    (org-glance-perf--install)))

(defun org-glance-perf--stage (name function &rest args)
  "Aggregate NAME's FUNCTION called with ARGS in the current trace."
  (if (null org-glance-perf--trace)
      (apply function args)
    (let ((start (float-time))
          (reported org-glance-perf--reporting))
      (unwind-protect
          (let ((org-glance-perf--depth (1+ org-glance-perf--depth)))
            (apply function args))
        (let ((elapsed (- (float-time) start
                          (- org-glance-perf--reporting reported)))
              (entry (or (gethash name org-glance-perf--trace)
                         (list 0 0.0))))
          (cl-incf (car entry))
          (cl-incf (cadr entry) elapsed)
          (puthash name entry org-glance-perf--trace)
          (when org-glance-perf-live
            (let ((report-start (float-time)))
              (message "glance-perf #%d live %s%s: %.1f ms"
                       org-glance-perf--current
                       (make-string (* 2 org-glance-perf--depth) ?\s)
                       name (* 1000 elapsed))
              (cl-incf org-glance-perf--reporting
                       (- (float-time) report-start)))))))))

(defun org-glance-perf--ready (serial start)
  "Report when trace SERIAL returns to the event loop after START."
  (when org-glance-perf-mode
    (message "glance-perf #%d event-loop +%.1f ms after command return"
             serial (* 1000 (- (float-time) start)))))

(defun org-glance-perf--command (name function &rest args)
  "Measure command NAME's FUNCTION with ARGS, preserving its result or error."
  (if org-glance-perf--trace
      (apply #'org-glance-perf--stage name function args)
    (let ((serial (cl-incf org-glance-perf--serial)))
      (message "glance-perf #%d %s start" serial name)
      (let* ((start (float-time))
           (gc-start gc-elapsed)
           (gc-count gcs-done)
           (org-glance-perf--trace (make-hash-table :test #'eq))
           (org-glance-perf--current serial)
           (org-glance-perf--reporting 0.0)
           (status 'aborted))
      (when (eq name 'org-glance-transient)
        (setq org-glance-perf--origin start))
      (unwind-protect
          (prog1 (apply function args) (setq status 'returned))
        (let* ((end (float-time))
               (gc-seconds (- gc-elapsed gc-start))
               (collections (- gcs-done gc-count))
               (elapsed (- end start org-glance-perf--reporting))
               (prompt (cadr (gethash 'read-from-minibuffer
                                     org-glance-perf--trace)))
               entries)
          (maphash (lambda (key value) (push (cons key value) entries))
                   org-glance-perf--trace)
          (dolist (entry (sort entries (lambda (a b) (> (caddr a) (caddr b)))))
            (message "glance-perf #%d   %s: %.1f ms / %d calls (inclusive)"
                     serial (car entry) (* 1000 (caddr entry)) (cadr entry)))
          (message
           "glance-perf #%d %s %s: %.1f ms; prompt %.1f; work %.1f; GC %.1f ms / %d%s"
           serial name status (* 1000 elapsed) (* 1000 (or prompt 0))
           (* 1000 (max 0 (- elapsed (or prompt 0))))
           (* 1000 gc-seconds) collections
           (if org-glance-perf--origin
               (format "; since transient %.1f ms (includes user input)"
                       (* 1000 (- end org-glance-perf--origin)))
             ""))
          (unless (memq name org-glance-perf-continuations)
            (setq org-glance-perf--origin nil))
          (when (and org-glance-perf-mode (not noninteractive))
            (let (timer)
              (setq timer
                    (run-at-time
                     0 nil (lambda ()
                             (setq org-glance-perf--timers
                                   (delq timer org-glance-perf--timers))
                             (org-glance-perf--ready serial end))))
              (push timer org-glance-perf--timers)))))))))

(defun org-glance-perf--install (&rest _)
  "Advise available entry points, including lazily loaded plugin functions."
  (dolist (name (append org-glance-perf-commands org-glance-perf-stages))
    (when (and (fboundp name) (not (assq name org-glance-perf--advices)))
      (let ((advice (apply-partially
                     (if (memq name org-glance-perf-commands)
                         #'org-glance-perf--command
                       #'org-glance-perf--stage)
                     name)))
        (advice-add name :around advice)
        (push (cons name advice) org-glance-perf--advices)))))

;;;###autoload
(define-minor-mode org-glance-perf-mode
  "Report verbose command latency and stage counts in *Messages*.
Measure transient setup, materialization, views and optional LLM launches.
Stage times are inclusive; prompt time includes minibuffer interaction.
The event-loop marker measures scheduling after return, including reporting.
Terminal creation ends at command return; provider readiness and first
model response require separate provider tracing."
  :global t
  :group 'org-glance
  (if org-glance-perf-mode
      (progn
        (org-glance-perf--install)
        (add-hook 'after-load-functions #'org-glance-perf--install))
    (remove-hook 'after-load-functions #'org-glance-perf--install)
    (org-glance-perf--clear-advices)
    (mapc #'cancel-timer org-glance-perf--timers)
    (setq org-glance-perf--advices nil
          org-glance-perf--timers nil
          org-glance-perf--origin nil)))

(when org-glance-perf-mode
  (org-glance-perf--clear-advices)
  (org-glance-perf-mode 1))

(provide 'org-glance-perf)
;;; org-glance-perf.el ends here
