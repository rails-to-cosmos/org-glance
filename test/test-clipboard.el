;;; test-clipboard.el --- Extraction confidentiality contracts -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-material)
(require 'org-glance-table)
(require 'org-glance-clipboard-osx)
(require 'org-glance-clipboard-linux-wayland)
(require 'org-glance-clipboard-linux-x)

(defconst org-glance-test--clipboard-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest org-glance-test:clipboard-mac-cleanup-race-model ()
  "Preserve concurrent copies under AppKit's conditional-write contract."
  (skip-unless (executable-find "node"))
  (with-temp-buffer
    (insert org-glance-clipboard-osx--prelude)
    (should (zerop (call-process-region
                    (point-min) (point-max) (executable-find "node") nil t nil
                    (expand-file-name "test-clipboard-osx-cleanup.js"
                                      org-glance-test--clipboard-directory))))))

(ert-deftest org-glance-test:clipboard-mac-native-cleanup-race ()
  "Preserve a native replacement between cleanup's final check and write."
  (skip-unless (eq system-type 'darwin))
  (let ((script (with-temp-buffer
                  (insert org-glance-clipboard-osx--prelude)
                  (insert-file-contents
                   (expand-file-name "clipboard-osx-cleanup-native.js"
                                     org-glance-test--clipboard-directory))
                  (buffer-string))))
    (with-temp-buffer
      (should (zerop (call-process
                      "/usr/bin/osascript" nil t nil "-l" "JavaScript" "-e" script
                      (format "org.glance.test.%s" (random most-positive-fixnum)))))
      (should (equal "ok" (string-trim (buffer-string)))))))

(ert-deftest org-glance-test:clipboard-native-linux-lifecycle ()
  "Run the Linux protocol, storage and process-lifetime contracts."
  (skip-unless (eq system-type 'gnu/linux))
  (skip-unless (executable-find "python3"))
  (with-temp-buffer
    (let ((status (call-process
                   (executable-find "python3") nil t nil "-B"
                   (expand-file-name "test-clipboard-native.py"
                                     org-glance-test--clipboard-directory))))
      (unless (zerop status) (ert-fail (buffer-string))))))

(ert-deftest org-glance-test:clipboard-delayed-failure-needs-ack ()
  "Refuse a helper that stays alive briefly and then fails publication."
  (skip-unless (executable-find "python3"))
  (should-error
   (org-glance-clipboard--send
    "credential" (list (executable-find "python3") "-c"
                       "import sys,time; sys.stdin.read(); time.sleep(.2); sys.exit(1)"))
   :type 'user-error))

(ert-deftest org-glance-test:clipboard-extract-then-confidential-paste ()
  "Paste an extracted value without importing it into the kill ring."
  (let ((kill-ring '("previous")) clipboard)
    (cl-letf (((symbol-function 'org-glance-clipboard:copy)
               (lambda (value) (setq clipboard value)))
              ((symbol-function 'gui-get-selection)
               (lambda (&rest _) clipboard)))
      (org-glance-material:extract-pairs '(("password" . "credential")) "password")
      (with-temp-buffer
        (buffer-enable-undo)
        (org-glance-clipboard:paste)
        (should (equal "credential" (buffer-string))))
      (setq clipboard nil)
      (should (equal '("previous") kill-ring)))))

(ert-deftest org-glance-test:clipboard-ordinary-paste-retains-value ()
  "Record the ordinary-yank limitation after extraction and clipboard expiry."
  (let ((kill-ring '("previous")) (kill-ring-yank-pointer nil) clipboard)
    (cl-letf (((symbol-function 'org-glance-clipboard:copy)
               (lambda (value) (setq clipboard value))))
      (org-glance-material:extract-pairs '(("password" . "credential")) "password"))
    (let ((interprogram-paste-function (lambda () clipboard)))
      (with-temp-buffer (yank)))
    (setq clipboard nil)
    (should (member "credential" kill-ring))))

(ert-deftest org-glance-test:extract-confidential ()
  "Copy ordinary properties without retaining or displaying their values."
  (let ((kill-ring '("previous")) copied messages)
    (cl-letf (((symbol-function 'org-glance-clipboard:copy)
               (lambda (value) (setq copied value)))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (should (equal "credential"
                     (org-glance-material:extract-pairs
                      '(("password" . "credential")) "password")))
      (should (equal copied "credential"))
      (should (equal kill-ring '("previous")))
      (should-not (string-match-p "credential" (format "%s" messages))))))

(ert-deftest org-glance-test:extract-decrypts-before-picker ()
  "Prompt for the password before offering encrypted property names."
  (let ((headline (org-glance-headline:encrypt
                   (org-glance-test:headline "secret" "* Secret"
                                             "- password: credential") "pw"))
        events copied)
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (push 'password events) "pw"))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (push 'picker events)
                 (should (assoc "password" choices))
                 "password"))
              ((symbol-function 'org-glance-clipboard:copy)
               (lambda (value) (setq copied value))))
      (org-glance-material:extract headline)
      (should (equal '(picker password) events))
      (should (equal "credential" copied)))))

(ert-deftest org-glance-test:extract-unknown-key ()
  "Refuse missing properties before touching the clipboard."
  (cl-letf (((symbol-function 'org-glance-clipboard:copy)
             (lambda (_) (ert-fail "Copied a missing property"))))
    (should-error (org-glance-material:extract-pairs '(("a" . "b")) "absent")
                  :type 'user-error)))

(ert-deftest org-glance-test:clipboard-unsupported-refuses ()
  "Refuse unsupported sessions without exporting through the kill ring."
  (let ((system-type 'gnu/linux) (window-system nil)
        (process-environment nil) (kill-ring '("previous")))
    (should-error (org-glance-clipboard:copy "secret") :type 'user-error)
    (should (equal kill-ring '("previous")))))

(ert-deftest org-glance-test:extract-wrong-password ()
  "Leave the clipboard untouched when decryption fails."
  (let ((headline (org-glance-headline:encrypt
                   (org-glance-test:headline "secret" "* Secret"
                                             "- password: credential") "pw")))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (copy-sequence "wrong")))
              ((symbol-function 'org-glance-clipboard:copy)
               (lambda (_) (ert-fail "Copied after failed decryption")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Picked before decryption"))))
      (should-error (org-glance-material:extract headline) :type 'user-error))))

(ert-deftest org-glance-test:extract-table-encrypted ()
  "Decrypt table extraction without caching plaintext or changing storage."
  (org-glance-test:with-graph graph
    (org-glance-graph:add
     graph (org-glance-headline:encrypt
            (org-glance-test:headline "secret" "* Secret"
                                      "- password: credential") "pw"))
    (let ((before (org-glance-graph:get-content graph "secret")) copied)
      (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (copy-sequence "pw")))
                ((symbol-function 'completing-read) (lambda (&rest _) "password"))
                ((symbol-function 'org-glance-clipboard:copy)
                 (lambda (value) (setq copied value))))
        (org-glance-table--act-extract graph "secret"))
      (should (equal "credential" copied))
      (should (equal before (org-glance-graph:get-content graph "secret")))
      (should-not (org-glance-property-index:body graph "secret")))))

(ert-deftest org-glance-test:extract-sealed-material ()
  "Extract from a sealed live buffer while preserving its contents and edits."
  (org-glance-test:with-graph graph
    (org-glance-graph:add
     graph (org-glance-headline:encrypt
            (org-glance-test:headline "secret" "* Secret"
                                      "- password: credential") "pw"))
    (org-glance-test:with-material (buffer graph "secret")
      (goto-char (point-max))
      (insert "\nPublic unsaved edit\n")
      (let ((before (buffer-string)) copied)
        (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (copy-sequence "pw")))
                  ((symbol-function 'completing-read) (lambda (&rest _) "password"))
                  ((symbol-function 'org-glance-clipboard:copy)
                   (lambda (value) (setq copied value))))
          (org-glance-material:extract-here))
        (should (equal "credential" copied))
        (should (equal before (buffer-string)))
        (should (buffer-modified-p))))))

(ert-deftest org-glance-test:extract-table-warm-work-contract ()
  "Read a cached ordinary property with zero blob reads or parses."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "plain" "* Plain" "- author: Tolkien"))
    (org-glance-property-index:body graph "plain")
    (cl-letf (((symbol-function 'org-glance-graph:get-content)
               (lambda (&rest _) (ert-fail "Warm extraction read a blob")))
              ((symbol-function 'org-glance-headline--from-string)
               (lambda (&rest _) (ert-fail "Warm extraction parsed a blob")))
              ((symbol-function 'completing-read) (lambda (&rest _) "author"))
              ((symbol-function 'org-glance-clipboard:copy)
               (lambda (value) (should (equal "Tolkien" value)))))
      (org-glance-table--act-extract graph "plain"))))

(ert-deftest org-glance-test:clipboard-helper-private-transport ()
  "Send Unicode and newlines through stdin without creating an output buffer."
  (skip-unless (executable-find "python3"))
  (let* ((secret "credential λ\nsecond line")
         (process
          (org-glance-clipboard--send
           secret
           (list (executable-find "python3") "-c"
                 "import sys,time; s=sys.stdin.read(); print('rea' if s == 'credential λ\\nsecond line' else 'bad',end='',flush=True); time.sleep(.01); print('dy',flush=True); time.sleep(30)")
           :ready "ready")))
    (unwind-protect
        (progn
          (should-not (process-buffer process))
          (should-not (member secret (process-command process)))
          (should (process-live-p process)))
      (org-glance-clipboard--stop process))
    (should-not (process-live-p process))))

(ert-deftest org-glance-test:clipboard-helper-failure-private ()
  "Suppress helper diagnostics that could contain plaintext."
  (skip-unless (executable-find "python3"))
  (should-error
   (org-glance-clipboard--send
    "credential" (list (executable-find "python3") "-c"
                       "import sys; print(sys.stdin.read(),flush=True); sys.exit(1)")
    :ready "ready")
   :type 'user-error))

(ert-deftest org-glance-test:clipboard-linux-independent-memory-helper ()
  "Use acknowledged memory helpers without wl-copy or Emacs expiry timers."
  (let ((system-type 'gnu/linux))
    (dolist (backend '("wayland" "x11"))
      (let ((process-environment
             (and (equal backend "wayland") '("WAYLAND_DISPLAY=test")))
            (window-system (and (equal backend "x11") 'x))
            sent command options)
        (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/python3"))
                  ((symbol-function 'org-glance-clipboard--send)
                   (lambda (value arguments &rest keywords)
                     (setq sent value command arguments options keywords)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (ert-fail "Expiry depends on Emacs timers"))))
          (org-glance-clipboard:copy "credential λ"))
        (should (equal sent (encode-coding-string "credential λ" 'utf-8-unix)))
        (should (equal (nth 4 command) backend))
        (should (equal (nth 5 command) (number-to-string (length sent))))
        (should (eq (plist-get options :coding) 'no-conversion))
        (should-not (string-match-p "credential" (mapconcat #'identity command " ")))))))

(ert-deftest org-glance-test:clipboard-mac-dispatch ()
  "Keep the secret out of JXA's arguments and require readiness acknowledgement."
  (let ((system-type 'darwin) sent command ready)
    (cl-letf (((symbol-function 'org-glance-clipboard--send)
               (lambda (value arguments &rest options)
                 (setq sent value command arguments ready (plist-get options :ready)))))
      (org-glance-clipboard:copy "credential"))
    (should (equal sent "credential"))
    (should-not (string-match-p "credential" (mapconcat #'identity command " ")))
    (should-not ready)
    (should (equal (car command) "/usr/bin/osascript"))))

(ert-deftest org-glance-test:clipboard-mac-native-expiry ()
  "Exercise AppKit markers and expiry on an isolated named macOS pasteboard."
  (skip-unless (eq system-type 'darwin))
  (let* ((org-glance-clipboard-ttl 3)
         (name (format "org.glance.test.%s" (random most-positive-fixnum)))
         (board (format "$.NSPasteboard.pasteboardWithName($('%s'))" name))
         (script (string-replace "$.NSPasteboard.generalPasteboard" board
                                 org-glance-clipboard-osx--script))
         (publisher (string-replace "$.NSPasteboard.generalPasteboard" board
                                    org-glance-clipboard-osx--publisher))
         (process (org-glance-clipboard--send
                   "test credential"
                   (list "/usr/bin/osascript" "-l" "JavaScript" "-e" script "3" publisher))))
    (unwind-protect
        (progn
          (with-temp-buffer
            (should
             (zerop
              (call-process
               "/usr/bin/osascript" nil t nil "-l" "JavaScript" "-e"
               (concat "ObjC.import('AppKit'); var b = " board ";"
                       "JSON.stringify({text:ObjC.unwrap(b.stringForType($.NSPasteboardTypeString)),"
                       "types:ObjC.deepUnwrap(b.types)})"))))
            (let ((result (json-parse-string (buffer-string) :object-type 'alist)))
              (should (equal "test credential" (alist-get 'text result)))
              (should (member "org.nspasteboard.ConcealedType"
                              (append (alist-get 'types result) nil)))
              (should (member "org.nspasteboard.TransientType"
                              (append (alist-get 'types result) nil)))))
          (let ((deadline (+ (float-time) 6)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (should-not (process-live-p process))
          (should (zerop (process-exit-status process)))
          (with-temp-buffer
            (should
             (zerop
              (call-process
               "/usr/bin/osascript" nil t nil "-l" "JavaScript" "-e"
               (concat "ObjC.import('AppKit'); var b = " board ";"
                       "!ObjC.unwrap(b.stringForType($.NSPasteboardTypeString))"))))
            (should (equal "true" (string-trim (buffer-string))))))
      (org-glance-clipboard--stop process))))

(defun org-glance-test:mac-lease ()
  "Start an isolated ten-second macOS lease and return its process and board."
  (let* ((board (format "$.NSPasteboard.pasteboardWithName($('org.glance.test.%s'))"
                        (random most-positive-fixnum)))
         (script (string-replace "$.NSPasteboard.generalPasteboard" board
                                 org-glance-clipboard-osx--script))
         (publisher (string-replace "$.NSPasteboard.generalPasteboard" board
                                    org-glance-clipboard-osx--publisher)))
    (list (org-glance-clipboard--send
           "test credential" (list "/usr/bin/osascript" "-l" "JavaScript" "-e"
                                   script "10" publisher))
          board)))

(defun org-glance-test:mac-eval (board expression)
  "Return EXPRESSION evaluated against the isolated pasteboard BOARD."
  (with-temp-buffer
    (should (zerop (call-process
                    "/usr/bin/osascript" nil t nil "-l" "JavaScript" "-e"
                    (concat "ObjC.import('AppKit'); var b = " board ";" expression))))
    (string-trim (buffer-string))))

(ert-deftest org-glance-test:clipboard-mac-native-helper-termination ()
  "Clear the isolated lease when either publisher or guardian is killed."
  (skip-unless (eq system-type 'darwin))
  (dolist (victim '(publisher guardian))
    (pcase-let ((`(,process ,board) (org-glance-test:mac-lease)))
      (unwind-protect
          (progn
            (if (eq victim 'guardian)
                (org-glance-clipboard--stop process)
              (let ((pid (with-temp-buffer
                           (should (zerop (call-process
                                           "pgrep" nil t nil "-P"
                                           (number-to-string (process-id process)))))
                           (string-to-number (buffer-string)))))
                (should (> pid 1))
                (signal-process pid 'SIGKILL)))
            (let ((deadline (+ (float-time) 3)) empty)
              (while (and (not empty) (< (float-time) deadline))
                (setq empty (equal "true" (org-glance-test:mac-eval
                                          board "!ObjC.unwrap(b.stringForType($.NSPasteboardTypeString))")))
                (unless empty (sleep-for .05)))
              (should empty)))
        (org-glance-clipboard--stop process)
        (org-glance-test:mac-eval board "b.clearContents; true")))))

(ert-deftest org-glance-test:clipboard-mac-native-replacement-preserved ()
  "Leave a subsequent pasteboard owner intact when the lease terminates."
  (skip-unless (eq system-type 'darwin))
  (pcase-let ((`(,process ,board) (org-glance-test:mac-lease)))
    (unwind-protect
        (progn
          (org-glance-test:mac-eval
           board "b.clearContents; b.setStringForType('replacement', $.NSPasteboardTypeString)")
          (let ((deadline (+ (float-time) 3)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process .05)))
          (should-not (process-live-p process))
          (should (equal "replacement" (org-glance-test:mac-eval
                                        board "ObjC.unwrap(b.stringForType($.NSPasteboardTypeString))"))))
      (org-glance-clipboard--stop process)
      (org-glance-test:mac-eval board "b.clearContents; true"))))
