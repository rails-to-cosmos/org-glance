;;; org-glance-clipboard.el --- Expiring confidential copies -*- lexical-binding: t -*-

;;; Commentary:
;; Clipboard API and process transport for property extraction.

;;; Code:

(require 'cl-lib)

(defgroup org-glance-clipboard nil
  "Confidential property copying."
  :group 'org-glance)

(defcustom org-glance-clipboard-ttl 15
  "Seconds an extracted property remains available for pasting."
  :type 'integer
  :group 'org-glance-clipboard)

(declare-function org-glance-clipboard-osx:copy "org-glance-clipboard-osx"
                  (value))
(declare-function org-glance-clipboard-linux-wayland:copy
                  "org-glance-clipboard-linux-wayland" (value))
(declare-function org-glance-clipboard-linux-x:copy "org-glance-clipboard-linux-x"
                  (value))

(defun org-glance-clipboard--stop (process)
  "Terminate PROCESS if it still owns a live clipboard source."
  (when (process-live-p process) (delete-process process)))

(cl-defun org-glance-clipboard--send
    (value command &key (ready "ready") (coding 'utf-8-unix))
  "Send VALUE to COMMAND using CODING; require its READY acknowledgment.
Discard helper diagnostics and refuse failed or unacknowledged publication."
  (let* ((ack nil)
         (matched 0)
         (expected (concat ready "\n"))
         (process (make-process
                   :name "org-glance-clipboard" :buffer nil
                   :command command :coding coding :noquery t
                   :connection-type 'pipe
                   :filter (lambda (_process output)
                             (when (and (numberp matched) (not ack))
                               (cl-loop for character across output
                                        while (numberp matched)
                                        do (if (and (< matched (length expected))
                                                    (= character (aref expected matched)))
                                               (cl-incf matched)
                                             (setq matched nil)))
                               (setq ack (equal matched (length expected)))))
                   :sentinel (lambda (_process _event))))
         (success nil))
    (unwind-protect
        (condition-case nil
            (progn
              (process-send-string process value)
              (process-send-eof process)
              (let ((deadline (+ (float-time) 5)))
                (while (and (not ack) (numberp matched) (process-live-p process)
                            (< (float-time) deadline))
                  (accept-process-output process 0.05)))
              (unless (and ack (process-live-p process))
                (user-error "Confidential clipboard publication failed"))
              (setq success t)
              process)
          (error (user-error "Confidential clipboard publication failed")))
      (unless success (org-glance-clipboard--stop process)))))

(defconst org-glance-clipboard--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the packaged clipboard helpers.")

(defun org-glance-clipboard--linux-copy (value backend)
  "Publish VALUE through the independent Linux BACKEND with a memory lease."
  (let ((python (executable-find "python3"))
        (helper (expand-file-name "org_glance_clipboard.py"
                                  org-glance-clipboard--directory))
        (bytes (encode-coding-string value 'utf-8-unix))
        (token (secure-hash 'sha256
                            (format "%s:%s:%s" (emacs-pid) (current-time) (random)))))
    (unless python (user-error "Confidential copying requires Python 3"))
    (unless (file-readable-p helper)
      (user-error "The org-glance clipboard helper is missing; reinstall the package"))
    (org-glance-clipboard--send
     bytes (list python "-I" "-B" helper backend
                 (number-to-string (length bytes))
                 (number-to-string org-glance-clipboard-ttl) token)
     :coding 'no-conversion)))

;;;###autoload
(defun org-glance-clipboard:paste ()
  "Insert clipboard text without importing it into the kill ring.
The destination buffer's undo and save policies apply to the inserted text.
The shared clipboard does not authenticate the source of the text."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((text (gui-get-selection
               'CLIPBOARD (if (memq window-system '(x pgtk)) 'UTF8_STRING 'STRING))))
    (unless (stringp text) (user-error "No text on the clipboard"))
    (insert (substring-no-properties text))))

(defun org-glance-clipboard:copy (value)
  "Copy VALUE confidentially, bypassing the kill ring and PRIMARY selection.
Expire after `org-glance-clipboard-ttl' seconds; refuse unsupported backends.
Confidentiality hints depend on clipboard-manager cooperation.
X11 sends directly from locked memory; macOS expires text through an
ownership-conditional write."
  (unless (and (integerp org-glance-clipboard-ttl)
               (> org-glance-clipboard-ttl 0))
    (user-error "Clipboard expiry must be a positive number of seconds"))
  (let ((inhibit-quit t))
    (cond
     ((eq system-type 'darwin)
      (require 'org-glance-clipboard-osx)
      (org-glance-clipboard-osx:copy value))
     ((getenv "WAYLAND_DISPLAY")
      (require 'org-glance-clipboard-linux-wayland)
      (org-glance-clipboard-linux-wayland:copy value))
     ((eq window-system 'x)
      (require 'org-glance-clipboard-linux-x)
      (org-glance-clipboard-linux-x:copy value))
     (t (user-error
         "Confidential copying requires macOS, Wayland or graphical X11"))))
  nil)

(provide 'org-glance-clipboard)
;;; org-glance-clipboard.el ends here
