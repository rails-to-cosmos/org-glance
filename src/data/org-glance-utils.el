;; -*- lexical-binding: t -*-

(require 'f)
(require 'aes)
(require 'dash)
(require 'org-element)

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance--present-string? (v)
  "Non-nil when V is a non-empty string.
Whitespace-only strings count as present (unlike `s-present?')."
  (and (stringp v) (not (string-empty-p v))))

(cl-defun org-glance--file-mtime (path)
  "Filesystem modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(cl-defun org-glance--read-eld (path)
  "Read the single Lisp form in the .eld PATH, or nil when absent/unreadable."
  (when (f-exists? path)
    (ignore-errors (car (read-from-string (f-read-text path 'utf-8))))))

(cl-defun org-glance--write-eld (path form)
  "Serialize FORM to the .eld PATH, creating parent dirs."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text (prin1-to-string form) 'utf-8 path))

(cl-defun org-glance--buffer-links ()
  (cl-loop for link-element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           for beg = (org-element-property :begin link-element)
           for end = (org-element-property :end link-element)
           for title = (substring-no-properties
                        (or (-some->> link-element
                              (org-element-contents)
                              (org-element-interpret-data))
                            (org-element-property :raw-link link-element)))
           for link = (s-trim (buffer-substring-no-properties beg end))
           collect title into titles
           collect link into links
           collect beg into positions
           finally return (-zip links titles positions)))

(cl-defun org-glance--buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-glance:key-value-pair-re nil t)
             for key = (s-trim (substring-no-properties (match-string 1)))
             for value = (s-trim (substring-no-properties (match-string 2)))
             collect (cons key value))))

(cl-defun org-glance--parse-links ()
  "Simple org-link parser, return list of cons cells (link . contents)."
  (cl-loop with descriptions = (cl-loop for (key . val) in (org-glance--buffer-key-value-pairs) collect (cons val key))
           with links = (org-glance--buffer-links)
           for (link title pos) in links
           for description = (alist-get link descriptions nil nil #'string=)
           when description
           collect (list link description pos)
           else
           collect (list link title pos)))

(defun org-glance--encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance--decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(defun org-glance--discard-buffer (buffer)
  "Kill BUFFER without the `Buffer modified; kill anyway?' confirmation.
For buffers org-glance owns and means to discard -- a capture temp file whose
content is already in the graph, a materialization abandoned on error -- the
modified flag is only noise, so clear it before killing.  No-op if BUFFER is
already dead."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun org-glance--kill-buffer-noconfirm ()
  "Clear the current buffer's modified flag and return t.
Install this buffer-locally on `kill-buffer-query-functions' for a buffer
org-glance owns and means to discard, so any code path that kills it -- the
interactive `C-c C-c' finalize, a programmatic finalize, or org-capture's own
teardown -- proceeds silently, with no `Buffer modified; kill anyway?'
confirmation.  Query functions run before that confirmation, so clearing the
flag here makes it a no-op."
  (set-buffer-modified-p nil)
  t)

(provide 'org-glance-utils)
