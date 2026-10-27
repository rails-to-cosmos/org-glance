;;; test-clipboard.el --- Extraction confidentiality contracts -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-material)
(require 'org-glance-table)
(require 'org-glance-clipboard-osx)
(require 'org-glance-clipboard-linux-wayland)
(require 'org-glance-clipboard-linux-x)

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

(ert-deftest org-glance-test:clipboard-expiry-ownership ()
  "Expire only the exact selection published by this copy."
  (let ((terminal (frame-terminal)) (token (copy-sequence "secret"))
        (owner t) current cleared)
    (cl-letf (((symbol-function 'gui-backend-selection-owner-p) (lambda (_) owner))
              ((symbol-function 'gui-get-selection) (lambda (&rest _) current))
              ((symbol-function 'gui-set-selection)
               (lambda (&rest _) (setq cleared t))))
      (setq current (copy-sequence token))
      (org-glance-clipboard-linux-x--expire terminal token)
      (should-not cleared)
      (setq current token)
      (setq owner nil)
      (org-glance-clipboard-linux-x--expire terminal token)
      (should-not cleared)
      (setq owner t)
      (org-glance-clipboard-linux-x--expire terminal token)
      (should cleared))))

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

(ert-deftest org-glance-test:clipboard-x-confidential-offer ()
  "Offer only CLIPBOARD with a conditional hint and an exact expiry token."
  (let ((selection-converter-alist (copy-tree selection-converter-alist))
        offered expiry)
    (cl-letf (((symbol-function 'gui-set-selection)
               (lambda (selection value) (setq offered (cons selection value))))
              ((symbol-function 'run-at-time)
               (lambda (&rest arguments) (setq expiry arguments))))
      (org-glance-clipboard-linux-x:copy "credential"))
    (should (eq 'CLIPBOARD (car offered)))
    (should (eq (cdr offered) (nth 4 expiry)))
    (should (= org-glance-clipboard-ttl (car expiry)))
    (should (equal '(x-kde-passwordManagerHint . "secret")
                   (org-glance-clipboard-linux-x--hint
                    'CLIPBOARD 'x-kde-passwordManagerHint (cdr offered))))
    (should-not (org-glance-clipboard-linux-x--hint
                 'PRIMARY 'x-kde-passwordManagerHint (cdr offered)))
    (should-not (org-glance-clipboard-linux-x--hint
                 'CLIPBOARD 'x-kde-passwordManagerHint "ordinary"))))

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

(ert-deftest org-glance-test:clipboard-wayland-dispatch ()
  "Pass sensitive content over stdin and expire only its source process."
  (let ((system-type 'gnu/linux) (process-environment '("WAYLAND_DISPLAY=test"))
        sent command expiry)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/wl-copy"))
              ((symbol-function 'org-glance-clipboard--send)
               (lambda (value arguments &rest _)
                 (setq sent value command arguments) 'source))
              ((symbol-function 'run-at-time)
               (lambda (&rest arguments) (setq expiry arguments))))
      (org-glance-clipboard:copy "credential"))
    (should (equal sent "credential"))
    (should (equal command '("wl-copy" "--foreground" "--sensitive"
                            "--type" "text/plain;charset=utf-8")))
    (should (equal expiry (list org-glance-clipboard-ttl nil
                               #'org-glance-clipboard--stop 'source)))))

(ert-deftest org-glance-test:clipboard-mac-dispatch ()
  "Keep the secret out of JXA's arguments and require readiness acknowledgement."
  (let ((system-type 'darwin) sent command ready)
    (cl-letf (((symbol-function 'org-glance-clipboard--send)
               (lambda (value arguments &rest options)
                 (setq sent value command arguments ready (plist-get options :ready)))))
      (org-glance-clipboard:copy "credential"))
    (should (equal sent "credential"))
    (should-not (string-match-p "credential" (mapconcat #'identity command " ")))
    (should (equal ready "ready"))
    (should (equal (car command) "/usr/bin/osascript"))))

(ert-deftest org-glance-test:clipboard-mac-native-expiry ()
  "Exercise AppKit markers and expiry on an isolated named macOS pasteboard."
  (skip-unless (eq system-type 'darwin))
  (let* ((org-glance-clipboard-ttl 3)
         (name (format "org.glance.test.%s" (random most-positive-fixnum)))
         (board (format "$.NSPasteboard.pasteboardWithName($('%s'))" name))
         (script (string-replace "$.NSPasteboard.generalPasteboard" board
                                 org-glance-clipboard-osx--script))
         (process (org-glance-clipboard--send
                   "test credential"
                   (list "/usr/bin/osascript" "-l" "JavaScript" "-e" script "3")
                   :ready "ready")))
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
