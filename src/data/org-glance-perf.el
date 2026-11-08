;;; org-glance-perf.el --- Interactive latency measurements -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `org-glance-perf-mode' to report command stages in *Messages*.

;;; Code:

(require 'cl-lib)
(require 'org-glance-core)

(defconst org-glance-perf--commands
  '(org-glance-transient org-glance-materialize org-glance-llm
    org-glance-llm-here org-glance-llm-sessions org-glance-overview
    org-glance-table org-glance-tags org-glance-open
    agnostic-llm--menu-open-session agnostic-llm)
  "Commands whose synchronous UX preparation is measured.")

(defconst org-glance-perf--stages
  '(org-glance-ensure-init org-glance-material:pick-metadata
    org-glance-material:completing-read org-glance-material--selection
    org-glance-cache:headlines
    org-glance-cache--read-db org-glance-cache--ordinary-rows
    org-glance-cache--read-projection org-glance-cache--decode-metadata
    org-glance-material--candidates org-glance-config:cache-key
    org-glance-material:open org-glance-cache:logical-id
    org-glance-cache:material-state org-glance-material--create
    org-glance-material--load-source org-glance-material--read-source
    org-glance-cache:incoming-relations org-glance-version:observe
    org-glance-property-index:property org-glance-llm--dir
    org-glance-llm--label org-glance-llm--open-session
    org-glance-llm--session-rows agnostic-llm-menu
    agnostic-llm--session-shell-command agnostic-llm--open-vterm
    agnostic-llm--named-session vterm pop-to-buffer transient-setup
    org-mode switch-to-buffer read-from-minibuffer)
  "Functions aggregated by call count and inclusive elapsed time.")

(defvar org-glance-perf--trace nil)
(defvar org-glance-perf--serial 0)
(defvar org-glance-perf--origin nil)
(defvar org-glance-perf--advices nil)
(defvar org-glance-perf--timers nil)
(defvar org-glance-perf-mode nil)

(defun org-glance-perf--stage (name function &rest args)
  "Aggregate NAME's FUNCTION called with ARGS in the current trace."
  (if (null org-glance-perf--trace)
      (apply function args)
    (let ((start (float-time)))
      (unwind-protect
          (apply function args)
        (let ((entry (or (gethash name org-glance-perf--trace)
                         (list 0 0.0))))
          (cl-incf (car entry))
          (cl-incf (cadr entry) (- (float-time) start))
          (puthash name entry org-glance-perf--trace))))))

(defun org-glance-perf--ready (serial start)
  "Report when trace SERIAL returns to the event loop after START."
  (when org-glance-perf-mode
    (message "glance-perf #%d event-loop +%.1f ms after command return"
             serial (* 1000 (- (float-time) start)))))

(defun org-glance-perf--command (name function &rest args)
  "Measure command NAME's FUNCTION with ARGS, preserving its result or error."
  (if org-glance-perf--trace
      (apply #'org-glance-perf--stage name function args)
    (let* ((start (float-time))
           (gc-start gc-elapsed)
           (gc-count gcs-done)
           (serial (cl-incf org-glance-perf--serial))
           (org-glance-perf--trace (make-hash-table :test #'eq))
           (status 'aborted))
      (when (eq name 'org-glance-transient)
        (setq org-glance-perf--origin start))
      (message "glance-perf #%d %s start" serial name)
      (unwind-protect
          (prog1 (apply function args) (setq status 'returned))
        (let* ((end (float-time))
               (gc-seconds (- gc-elapsed gc-start))
               (collections (- gcs-done gc-count))
               (elapsed (- end start))
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
          (unless (memq name '(org-glance-transient org-glance-llm
                               org-glance-llm-here))
            (setq org-glance-perf--origin nil))
          (when (and org-glance-perf-mode (not noninteractive))
            (let (timer)
              (setq timer
                    (run-at-time
                     0 nil (lambda ()
                             (setq org-glance-perf--timers
                                   (delq timer org-glance-perf--timers))
                             (org-glance-perf--ready serial end))))
              (push timer org-glance-perf--timers))))))))

(defun org-glance-perf--install (&rest _)
  "Advise available entry points, including lazily loaded plugin functions."
  (dolist (name (append org-glance-perf--commands org-glance-perf--stages))
    (when (and (fboundp name) (not (assq name org-glance-perf--advices)))
      (let ((advice (apply-partially
                     (if (memq name org-glance-perf--commands)
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
    (dolist (entry org-glance-perf--advices)
      (advice-remove (car entry) (cdr entry)))
    (mapc #'cancel-timer org-glance-perf--timers)
    (setq org-glance-perf--advices nil
          org-glance-perf--timers nil
          org-glance-perf--origin nil)))

(provide 'org-glance-perf)
;;; org-glance-perf.el ends here
