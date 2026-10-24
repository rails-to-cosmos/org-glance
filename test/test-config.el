;;; test-config.el --- Typed settings and canonical Org context -*- lexical-binding: t -*-
(require 'ert)
(require 'org-glance-config)

(defun org-glance-config-test--publish (graph text)
  "Publish JSON TEXT in GRAPH and refresh its live observation."
  (org-glance-test:write
   (org-glance-config-file:path (org-glance-graph:directory graph)) text)
  (org-glance-config:refresh graph))

(ert-deftest org-glance-config-test-finalize-allocates-notification-locally ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let ((org-glance-graph graph))
      (org-glance-capture 'notification "New notice" :finalize t))
    (let* ((rows (org-glance-graph:headlines graph))
           (id (org-glance-headline-metadata:id (car rows)))
           (path (org-glance-cache:material-path graph id)))
      (should (= 1 (length rows)))
      (should (equal "New notice" (org-glance-headline-metadata:title (car rows))))
      (should (f-ancestor-of? (org-glance-graph:local-data-path graph) path))
      (should (string-match-p "New notice" (f-read-text path 'utf-8-unix)))
      (let* ((copy (org-glance-material:duplicate graph id))
             (copy-path (org-glance-cache:material-path graph copy)))
        (should (f-ancestor-of? (org-glance-graph:local-data-path graph) copy-path))))))

(ert-deftest org-glance-config-test-finalize-ingest-failure-retains-draft ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let ((org-glance-graph graph))
      (org-glance-capture 'notification "Recover this notice")
      (let* ((target (or (buffer-base-buffer) (current-buffer)))
             (path (buffer-file-name target)))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'org-glance-version:write-snapshot)
                         (lambda (&rest _) (error "Injected ingest failure"))))
                (should-error (org-capture-finalize)))
              (should (buffer-live-p target))
              (should (file-exists-p path))
              (should (string-match-p "Recover this notice" (f-read-text path 'utf-8-unix)))
              (with-current-buffer target
                (should (string-match-p "Recover this notice" (buffer-string)))
                (should (buffer-modified-p))
                (goto-char (point-min))
                (org-next-visible-heading 1)
                (should (org-entry-get nil "ORG_GLANCE_ID"))
                (should-not (memq #'org-glance--kill-buffer-noconfirm kill-buffer-query-functions)))
              (should-not (org-glance-graph:headlines graph))
              (org-glance-graph:capture graph target)
              (should (= 1 (length (org-glance-graph:headlines graph)))))
          (org-glance--discard-buffer target)
          (when (file-exists-p path) (delete-file path)))))))

(ert-deftest org-glance-config-test-storage-folds-both-sides-without-changing-scope ()
  (dolist (pair '(("ΑΣ" . "ΑΣ") ("straße" . "STRASSE") ("STRASSE" . "straße")
                  ("Σ" . "ς") ("Ꭰ" . "ꭰ")))
    (org-glance-test:with-graph graph
      (org-glance-config-test--publish graph
       (format "{\"format\":1,\"tags\":{\"%s\":{\"storage\":\"local\"}}}" (car pair)))
      (org-glance-test:capture graph (format "* Local title :%s:\n" (cdr pair)))
      (let* ((row (car (org-glance-graph:headlines graph)))
             (path (org-glance-cache:material-path graph (org-glance-headline-metadata:id row))))
        (should (f-ancestor-of? (org-glance-graph:local-data-path graph) path)))
      (let* ((id "portable-family")
             (original (org-glance-test:headline id "* Portable :other:")))
        (org-glance-graph:add graph original)
        (let* ((path (org-glance-cache:material-path graph id))
               (bytes (f-read-text path 'utf-8-unix))
               (replacement (org-glance-test:headline id (format "* Retagged :%s:" (cdr pair)))))
          (should-error (org-glance-graph:put-content graph replacement) :type 'user-error)
          (should (equal bytes (f-read-text path 'utf-8-unix)))
          (should (= 1 (length (org-glance-version:history
                                (org-glance-graph:headline-data-path graph id) id)))))))))

(ert-deftest org-glance-config-test-runtime-authority-and-errors ()
  (org-glance-test:with-graph graph
    (org-glance-test:write
     (f-join dir ".org-glance" "config" "system.org") "#+TODO: OLD | PLAN\n")
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}],\"tree\":{\"headlineHistoryDepth\":3}}")
    (let ((settings (org-glance-config:current graph)))
      (should (= 3 (org-glance-graph--headline-history-depth graph)))
      (should (string-match-p "PLAN" (org-glance-tag-config:cycle-for-filter graph nil)))
      (should-not (string-match-p "OLD" (org-glance-tag-config:cycle-for-filter graph nil)))
      (cl-letf (((symbol-function 'display-warning) #'ignore))
        (org-glance-config-test--publish graph "{broken"))
      (should (eq settings (org-glance-config:current graph)))
      (should (org-glance-graph:-config-error graph))
      (org-glance-config-test--publish graph "{\"format\":1}")
      (should-not (org-glance-graph:-config-error graph)))))

(ert-deftest org-glance-config-test-cold-invalid-does-not-fallback ()
  (with-temp-directory dir
    (org-glance-test:write (f-join dir ".org-glance" "config.json") "{broken")
    (should-error (org-glance-graph dir) :type 'org-glance-config-error)))

(ert-deftest org-glance-config-test-runtime-writes-pin-and-preserve ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"clients\":{\"other\":{\"keep\":true}}}")
    (let* ((before (org-glance-config:snapshot graph))
           (revision (org-glance-config-file:revision before)))
      (org-glance-config:set-tree-setting graph "inboxTag" "new" :expected revision)
      (should-error (org-glance-config:set-tree-setting graph "inboxTag" "stale" :expected revision)
                    :type 'org-glance-config-conflict)
      (let ((snapshot (org-glance-config:snapshot graph)))
        (should (gethash "keep" (gethash "other" (gethash "clients" (org-glance-config-file:object snapshot)))))
        (should-error (org-glance-config:set-tree-setting graph "headlineHistoryDepth" -1
                       :expected (org-glance-config-file:revision snapshot)) :type 'org-glance-config-error)
        (should (eq snapshot (org-glance-config:snapshot graph)))))))

(ert-deftest org-glance-config-test-presentation-reload-does-not-reparse ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let ((id "json-warm"))
      (org-glance-version:write-snapshot (org-glance-graph:headline-data-path graph id)
       id nil "test" (org-glance-test:org-with-id "* TODO Warm" id) 10))
    (org-glance-cache:reconcile-inventory graph t)
    (should (= 1 (length (org-glance-cache:headlines graph))))
    (let ((key (org-glance-config:cache-key graph)))
      (cl-letf (((symbol-function 'org-glance-graph--reparse-blob)
                 (lambda (&rest _) (ert-fail "Presentation reload reparsed a Snapshot")))
                ((symbol-function 'org-glance-cache:reconcile-inventory)
                 (lambda (&rest _) (ert-fail "Presentation reload rebuilt the cache"))))
        (org-glance-config-test--publish graph
         "{\"format\":1,\"template\":\"* TODO New\",\"tree\":{\"headlineHistoryDepth\":0}}"))
      (should (equal key (org-glance-config:cache-key graph)))
      (should (org-glance-cache--configuration-current-p graph))
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (let ((generation (sqlite-select db "PRAGMA data_version")))
              (remhash (org-glance-graph:directory graph) org-glance-graph:list)
              (cl-letf (((symbol-function 'org-glance-graph--reparse-blob)
                         (lambda (&rest _) (ert-fail "Warm startup reparsed a Snapshot")))
                        ((symbol-function 'org-glance-cache:reconcile-inventory)
                         (lambda (&rest _) (ert-fail "Warm startup published a generation"))))
                (let ((reopened (org-glance-graph dir)))
                  (should (= 1 (length (org-glance-cache:headlines reopened))))))
              (should (equal generation (sqlite-select db "PRAGMA data_version"))))
          (sqlite-close db))))))

(ert-deftest org-glance-config-test-classification-rebuilds-an-atomic-generation ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let* ((id "json-reclassify")
           (family (org-glance-graph:headline-data-path graph id)))
      (org-glance-version:write-snapshot family id nil "test"
       (org-glance-test:org-with-id "* PLAN Title" id) 10)
      (org-glance-cache:reconcile-inventory graph t)
      (should (equal "PLAN Title" (org-glance-headline-metadata:title (org-glance-cache:headline graph id))))
      (org-glance-config-test--publish graph
       "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}")
      (should-not (org-glance-graph:-config-error graph))
      (should (org-glance-cache--configuration-current-p graph))
      (let ((row (org-glance-cache:headline graph id)))
        (should (equal "Title" (org-glance-headline-metadata:title row)))
        (should (equal "PLAN" (org-glance-headline-metadata:state row)))))))

(ert-deftest org-glance-config-test-native-context-keeps-cycles-and-source ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\",\"WORK(w)\"],\"inactive\":[\"FINISHED(f)\"]}]}")
    (with-temp-buffer
      (insert "#+TODO: WAIT(q) | END(e)\n* PLAN Title\n")
      (org-mode)
      (setq-local org-glance-config--org-context (org-glance-config:org-context graph nil))
      (let ((before (buffer-string)))
        (org-set-regexps-and-options)
        (should (member "PLAN" org-not-done-keywords))
        (should-not (member "PLAN" org-done-keywords))
        (should (member '("PLAN" "WORK" "FINISHED") org-todo-sets))
        (should (member '("WAIT" "END") org-todo-sets))
        (should (equal before (buffer-string)))
        (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
        (org-set-regexps-and-options)
        (should (equal before (buffer-string)))))))

(ert-deftest org-glance-config-test-peer-generation-requires-reconciliation ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (org-glance-cache:reconcile-inventory graph t)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (sqlite-execute db "UPDATE configuration_dependency SET fingerprint='peer'")
        (sqlite-close db)))
    (should-error (org-glance-cache:headlines graph))
    (org-glance-cache:reconcile-inventory graph)
    (should (org-glance-cache--configuration-current-p graph))))

(ert-deftest org-glance-config-test-incremental-writer-cannot-mint-census-dependency ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let ((id "json-incremental"))
      (org-glance-version:write-snapshot (org-glance-graph:headline-data-path graph id)
       id nil "test" (org-glance-test:org-with-id "* TODO Title" id) 10)
      (org-glance-cache:reconcile-inventory graph t)
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect (sqlite-execute db "DELETE FROM configuration_dependency")
          (sqlite-close db)))
      (org-glance-cache:reconcile-families graph (list id))
      (should-not (org-glance-cache--configuration-current-p graph))
      (should-error (org-glance-cache:headlines graph))
      (org-glance-cache:reconcile-inventory graph)
      (should (org-glance-cache--configuration-current-p graph)))))

(ert-deftest org-glance-config-test-publication-rejects-changed-json-revision ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (org-glance-cache:reconcile-inventory graph t)
    (let ((org-glance-cache--recovery-publication-hook
           (lambda ()
             (org-glance-test:write (org-glance-config-file:path dir)
              "{\"format\":1,\"tree\":{\"inboxTag\":\"changed\"}}"))))
      (should-error (org-glance-cache:reconcile-inventory graph t)))
    (should (org-glance-cache--configuration-current-p graph))
    (org-glance-config:refresh graph)
    (should (equal "changed" (gethash "inboxTag" (org-glance-settings:tree (org-glance-config:current graph)))))))

(ert-deftest org-glance-config-test-unchanged-observation-reuses-typed-settings ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let ((before (org-glance-config:current graph)))
      (cl-letf (((symbol-function 'org-glance-config:decode)
                 (lambda (&rest _) (ert-fail "Unchanged JSON decoded again"))))
        (should (eq before (org-glance-config:refresh graph)))))))

(ert-deftest org-glance-config-test-notifications-remain-local-and-capture-uses-json ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"template\":\"* PLAN %TITLE% :%TAGS%:\\n#+begin_src org\\n#+TODO: FAKE | FALSE\\n#+end_src\",\"todoCycles\":[{\"active\":[\"PLAN\"]}]}")
    (let* ((org-glance-graph graph)
           (template (org-glance-capture:template 'notification "Title"))
           (headline (org-glance-test:headline "json-local" "* TODO Notice :notification:"))
           (path (org-glance-graph:put-content graph headline)))
      (should (string-prefix-p "#+TODO:" template))
      (should (string-match-p "PLAN" template))
      (should (f-ancestor-of? (org-glance-graph:local-data-path graph) path)))))

(ert-deftest org-glance-config-test-native-material-cycles-and-saves-canonical-source ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\",\"WORK\"],\"inactive\":[\"FINISHED\"]}]}")
    (let* ((id "json-material")
           (family (org-glance-graph:headline-data-path graph id)))
      (org-glance-version:write-snapshot family id nil "test"
       (org-glance-test:org-with-id "* PLAN Title" id) 10)
      (org-glance-cache:reconcile-inventory graph t)
      (org-glance-test:with-material (buffer graph id)
        (should (member "PLAN" org-not-done-keywords))
        (should (member '("PLAN" "WORK" "FINISHED") org-todo-sets))
        (goto-char (point-min))
        (let ((org-inhibit-logging t)) (org-todo "WORK"))
        (org-glance-test:save)
        (let ((saved (f-read-text buffer-file-name 'utf-8-unix)))
          (should (string-prefix-p "* WORK Title" saved))
          (should-not (string-match-p "#\\+TODO:" saved)))
        (let ((source (buffer-string)) (point (point)))
          (org-glance-config-test--publish graph
           "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\",\"WORK\",\"NEW\"],\"inactive\":[\"FINISHED\"]}]}")
          (should (member "NEW" org-not-done-keywords))
          (should (equal source (buffer-string)))
          (should (= point (point))))))))

(ert-deftest org-glance-config-test-native-material-saves-exact-newline-bytes ()
  (dolist (endings '(("\r\n") ("\n") ("\r\n" "\n") ("\n" "\r\n")))
    (dolist (final-newline '(nil t))
      (dolist (author '(nil ":AUTHOR: Reader"))
        (org-glance-test:with-graph graph
          (org-glance-config-test--publish graph
            "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\",\"WORK\"],\"inactive\":[\"DONE\"]}]}")
          (let* ((id "json-material-crlf")
                 (family (org-glance-graph:headline-data-path graph id))
                 (index -1)
                 (source (mapconcat
                          (lambda (line)
                            (concat line (nth (mod (cl-incf index) (length endings)) endings)))
                          (append (list "* PLAN Tïtle" ":PROPERTIES:"
                                        (concat ":ORG_GLANCE_ID: " id))
                                  (when author (list author))
                            (list ":END:" "" "Keep α" "Change β")) ""))
                 (source (if final-newline source
                           (substring source 0 (- (length (nth (mod index (length endings)) endings)))))))
            (org-glance-version:write-snapshot family id nil "test" source 10)
            (org-glance-cache:reconcile-inventory graph t)
            (org-glance-test:with-material (buffer graph id)
              (let ((path buffer-file-name))
                (should-not (string-match-p "\r" (buffer-string)))
                (set-buffer-modified-p t)
                (cl-letf (((symbol-function 'org-glance-version:write-snapshot)
                           (lambda (&rest _) (error "Injected publication failure"))))
                  (should-error (org-glance-test:save)))
                (should (buffer-modified-p))
                (should (equal source (f-read-text path 'utf-8-unix)))
                (org-glance-test:save)
                (should (equal source (f-read-text buffer-file-name 'utf-8-unix)))
                (revert-buffer t t)
                (should-not (string-match-p "ORG_GLANCE_ID" (buffer-string)))
                (goto-char (point-min))
                (let ((org-inhibit-logging t)) (org-todo "WORK"))
                (goto-char (point-min)) (search-forward "Change β")
                (replace-match "Changed γ" t t)
                (let ((expected (replace-regexp-in-string
                                 "Change β" "Changed γ"
                                 (replace-regexp-in-string "PLAN" "WORK" source t t) t t)))
                  (org-glance-test:save)
                  (should (equal expected (f-read-text buffer-file-name 'utf-8-unix)))
                  (should-not (string-match-p "#\\+TODO:" expected))
                  (revert-buffer t t)
                  (goto-char (point-max))
                  (unless final-newline (insert "\n"))
                  (insert "New δ\n")
                  (org-glance-test:save)
                  (should (equal (concat expected (unless final-newline (car endings))
                                         "New δ" (car endings))
                                 (f-read-text buffer-file-name 'utf-8-unix))))
                (should (equal source (f-read-text path 'utf-8-unix)))
                (should (= 4 (length (org-glance-version:history family id))))))))))))

(ert-deftest org-glance-config-test-crlf-parser-preserves-selected-subtree ()
  (let* ((body "* TODO First\r\n:PROPERTIES:\r\n:ORG_GLANCE_ID: first\r\n:AUTHOR: Reader\n:END:\r\nText\n** Child\r\nChild body\r\n")
         (source (concat "#+TITLE: Preamble\r\n" body "* TODO Second\r\nOther\r\n"))
         (headline (org-glance-headline--from-string source))
         (lf (org-glance-headline--from-string
              (replace-regexp-in-string "\r\n" "\n" body t t))))
    (should (equal "first" (org-glance-headline:id headline)))
    (should (equal body (org-glance-headline:contents headline)))
    (should (equal "Reader" (org-glance-headline:node-property "AUTHOR" headline)))
    (should (equal (org-glance-headline:hash lf) (org-glance-headline:hash headline)))
    (should (equal (org-glance-headline--content-facts lf)
                   (org-glance-headline--content-facts headline)))))

(ert-deftest org-glance-config-test-lf-endings-need-no-extra-source-read ()
  (with-temp-buffer
    (insert "* TODO Ordinary LF\nBody\n")
    (setq-local buffer-file-coding-system 'utf-8-unix)
    (cl-letf (((symbol-function 'f-read-text)
               (lambda (&rest _) (ert-fail "LF initialization reread the source"))))
      (org-glance-material--read-endings))
    (should-not org-glance-material--newline-mixed)
    (should-not (text-property-not-all (point-min) (point-max) 'org-glance-newline nil))
    (should-not (org-glance-material--ending-annotations (point-min) (point-max)))))

(ert-deftest org-glance-config-test-crlf-repetition-retains-authored-bytes ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"],\"inactive\":[\"DONE\"]}]}")
    (let* ((id "crlf-repeat")
           (family (org-glance-graph:headline-data-path graph id))
           (source (replace-regexp-in-string
                    "\n" "\r\n"
                    (org-glance-test:org-with-id
                     "* PLAN Repeat\nSCHEDULED: <2026-06-07 Sun +1d>" id
                     "#+begin_pin\nKeep α\n#+end_pin\nTransient β") t t)))
      (org-glance-version:write-snapshot family id nil "test" source 10)
      (org-glance-cache:reconcile-inventory graph t)
      (org-glance-test:with-repeat (buffer graph id 2)
        (org-glance-test:complete-repetition)
        (let ((occurrence (f-read-text (cdar (org-glance-graph:occurrences graph id)) 'utf-8-unix)))
          (should (string-match-p "Transient β\r\n" occurrence))
          (should-not (string-match-p "#\\+TODO:" occurrence)))
        (org-glance-test:save)
        (revert-buffer t t)
        (org-glance-test:save)
        (let ((saved (f-read-text buffer-file-name 'utf-8-unix)))
          (should (string-prefix-p "* PLAN Repeat\r\n" saved))
          (should (string-match-p "Keep α\r\n" saved))
          (should-not (string-match-p "Transient β\\|#\\+TODO:" saved)))
        (should (= 1 (length (org-glance-graph:headlines graph))))))))

(ert-deftest org-glance-config-test-repetition-preserves-retained-mixed-endings ()
  (dolist (default '("\n" "\r\n"))
    (org-glance-test:with-graph graph
      (org-glance-config-test--publish graph
       "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"],\"inactive\":[\"DONE\"]}]}")
      (let* ((exception (if (equal default "\n") "\r\n" "\n"))
             (id "mixed-repeat")
             (family (org-glance-graph:headline-data-path graph id))
             (kept (concat "Keep α" exception "Keep β" default))
             (author (concat ":AUTHOR: Reader" exception))
             (source (concat "* PLAN Repeat" default
                             "SCHEDULED: <2026-06-07 Sun +1d>" exception
                             ":PROPERTIES:" default
                             ":ORG_GLANCE_ID: " id default author ":END:" default
                             "#+begin_pin" default kept "#+end_pin" default
                             "Transient γ" default))
             (original (org-glance-version:write-snapshot family id nil "test" source 10))
             (original-path (f-join (org-glance-version:directory original) "data.org")))
        (org-glance-cache:reconcile-inventory graph t)
        (org-glance-test:with-repeat (buffer graph id 2)
          (dotimes (iteration 2)
            (org-glance-test:complete-repetition)
            (let ((occurrence (f-read-text
                               (cdar (org-glance-graph:occurrences graph id)) 'utf-8-unix)))
              (should (string-search kept occurrence))
              (should (string-search author occurrence))
              (should-not (string-search "#+TODO:" occurrence)))
            (org-glance-test:save)
            (revert-buffer t t)
            (set-buffer-modified-p t)
            (org-glance-test:save)
            (let ((saved (f-read-text buffer-file-name 'utf-8-unix)))
              (should (string-search kept saved))
              (should (string-search author saved))
              (should-not (string-search "Transient γ" saved))
              (should-not (string-search "#+TODO:" saved)))
            (should (equal "PLAN" (org-get-todo-state)))
            (should (= (1+ iteration) (length (org-glance-graph:occurrences graph id))))))
        (should (equal source (f-read-text original-path 'utf-8-unix)))
        (should (= 1 (length (org-glance-graph:headlines graph))))))))

(ert-deftest org-glance-config-test-material-preserves-unrelated-unsaved-buffer ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (org-glance-graph:add graph (org-glance-test:headline "unsaved-source" "* TODO Source"))
    (let* ((path (org-glance-cache:material-path graph "unsaved-source"))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-max)) (insert "Unsaved draft")
            (let ((draft (buffer-string)))
              (should-error (org-glance-material:open graph "unsaved-source") :type 'user-error)
              (should (equal draft (buffer-string)))
              (should (buffer-modified-p))))
        (org-glance--discard-buffer buffer)))))

(defun org-glance-config-test--settings (text)
  "Decode UTF-8 JSON TEXT into typed settings."
  (org-glance-config:decode
   (org-glance-config-file:decode (encode-coding-string text 'utf-8-unix))))

(defun org-glance-config-test--snapshot (graph id source)
  "Publish SOURCE as ID's immutable Snapshot in GRAPH and reconcile it."
  (let* ((family (org-glance-graph:headline-data-path graph id))
         (version (org-glance-version:write-snapshot family id nil "test" source 10)))
    (org-glance-cache:reconcile-inventory graph t)
    (f-join (org-glance-version:directory version) "data.org")))

(ert-deftest org-glance-config-test-overview-source-context-is-metadata-only ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}],\"tags\":{\"book\":{\"todoCycles\":[{\"active\":[\"WORK(w)\"]}]}}}")
    (let* ((source (org-glance-test:org-with-id "* FILE Title :book:" "context-row"
                    "#+SEQ_TODO: FILE(f!) | FINISHED(x@)\nBody α"))
           (path (org-glance-config-test--snapshot graph "context-row" source)))
      (cl-letf (((symbol-function 'org-glance-graph:get-content) (lambda (&rest _) (ert-fail "Blob read")))
                ((symbol-function 'org-glance-headline--from-string) (lambda (&rest _) (ert-fail "Headline parse")))
                ((symbol-function 'org-glance-tag-config:todo-declarations) (lambda (&rest _) (ert-fail "Source context parse"))))
        (let ((text (org-glance-overview:render graph '(:done nil))))
          (with-temp-buffer
            (insert text) (org-glance--org-mode) (goto-char (point-min))
            (re-search-forward "^\\* ") (beginning-of-line)
            (should (equal "FILE" (org-get-todo-state)))
            (should (member "FILE" org-not-done-keywords))
            (should (member "FINISHED" org-done-keywords))
            (should (equal ?f (cdr (assoc "FILE" org-todo-key-alist)))))))
      (should (equal source (f-read-text path 'utf-8-unix))))))

(ert-deftest org-glance-config-test-overview-refuses-mixed-after-state-filter ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"tags\":{\"book\":{\"todoCycles\":[{\"active\":[\"PLAN\"],\"inactive\":[\"DONE\"]}]},\"other\":{\"todoCycles\":[{\"active\":[\"DONE\"],\"inactive\":[\"PLAN\"]}]}}}")
    (org-glance-config-test--snapshot graph "active-row"
      (org-glance-test:org-with-id "* PLAN Active :book:" "active-row"))
    (org-glance-config-test--snapshot graph "done-row"
      (org-glance-test:org-with-id "* PLAN Inactive :other:" "done-row"))
    (let ((error (should-error (org-glance-overview:render graph) :type 'org-glance-config-error)))
      (should (string-search "active-row" (error-message-string error)))
      (should (string-search "done-row" (error-message-string error))))
    (should (string-search "* PLAN Active" (org-glance-overview:render graph '(:done nil))))
    (should-not (string-search "* PLAN Inactive" (org-glance-overview:render graph '(:done nil))))
    (should (string-search "* PLAN Inactive" (org-glance-overview:render graph '(:done t))))
    (should (string-search "* PLAN Active" (org-glance-overview:render graph '(:tags ("book")))))))

(ert-deftest org-glance-config-test-overview-reload-receipt-and-refusal-are-coherent ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}")
    (org-glance-config-test--snapshot graph "cache-row"
      (org-glance-test:org-with-id "* PLAN Cache :book:" "cache-row"))
    (let* ((file (org-glance-overview:cached-file graph))
           (first (f-read-text file 'utf-8)))
      (cl-letf (((symbol-function 'org-glance-cache:overview-rows)
                 (lambda (&rest _) (ert-fail "Warm Overview fetched Rows")))
                ((symbol-function 'org-glance-config:fingerprints)
                 (lambda (&rest _) (ert-fail "Warm Overview recomputed fingerprints"))))
        (should (equal file (org-glance-overview:cached-file graph))))
      (cl-letf (((symbol-function 'org-glance-cache:reconcile-inventory)
                 (lambda (&rest _) (ert-fail "Presentation rebuilt Rows"))))
        (org-glance-config-test--publish graph
         "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(q)\"]}]}"))
      (let ((previous file))
        (setq file (org-glance-overview:cached-file graph))
        (should-not (equal previous file))
        (should (equal first (f-read-text previous 'utf-8))))
      (should-not (equal first (f-read-text file 'utf-8)))
      (should (string-search "PLAN(q)" (f-read-text file 'utf-8)))
      (org-glance-config-test--snapshot graph "other-row"
        (org-glance-test:org-with-id "* PLAN Other :other:" "other-row"))
      (setq file (org-glance-overview:cached-file graph))
      (let ((bytes (f-read-text file 'utf-8))
            (receipt (f-read-text (org-glance-overview--spec-sidecar file) 'utf-8)))
        (org-glance-config-test--publish graph
         "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(q)\"]}],\"tags\":{\"other\":{\"todoCycles\":[{\"active\":[\"WAIT(w)\"]}]}}}")
        (should-error (org-glance-overview:cached-file graph) :type 'org-glance-config-error)
        (should (equal bytes (f-read-text file 'utf-8)))
        (should (equal receipt (f-read-text (org-glance-overview--spec-sidecar file) 'utf-8)))))))

(ert-deftest org-glance-config-test-overview-receipts-survive-rebuild-and-family-only-change ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph "{\"format\":1}")
    (let* ((id "receipt-row")
           (family (org-glance-graph:headline-data-path graph id))
           (first (org-glance-config-test--snapshot graph id
                    (org-glance-test:org-with-id "* FIRST First" id "#+TODO: FIRST(f) | FINISHED")))
           (file (org-glance-overview:cached-file graph))
           (compatibility (org-glance--file-mtime (org-glance-graph:headline-meta-path graph))))
      (org-glance-version:write-snapshot family id
       (mapcar #'org-glance-version:id (org-glance-version:leaves (org-glance-version:history family id)))
       "test" (org-glance-test:org-with-id "* SECOND Second" id "#+TODO: SECOND(s) | FINISHED") 10)
      (org-glance-cache:reconcile-inventory graph t)
      (should (equal compatibility (org-glance--file-mtime (org-glance-graph:headline-meta-path graph))))
      (let ((previous file))
        (setq file (org-glance-overview:cached-file graph))
        (should-not (equal previous file))
        (should (string-search "FIRST(f)" (f-read-text previous 'utf-8))))
      (should (string-search "SECOND(s)" (f-read-text file 'utf-8)))
      (should-not (string-search "FIRST(f)" (f-read-text file 'utf-8)))
      (org-glance-cache:recover graph)
      (cl-letf (((symbol-function 'org-glance-tag-config:todo-declarations)
                 (lambda (&rest _) (ert-fail "Rebuilt Overview reparsed source"))))
        (should (string-search "SECOND(s)" (org-glance-overview:render graph))))
      (should (string-search "FIRST" (f-read-text first 'utf-8-unix))))))

(ert-deftest org-glance-config-test-overview-refresh-restores-row-and-native-context ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}")
    (dolist (id '("one" "two"))
      (org-glance-config-test--snapshot graph id
        (org-glance-test:org-with-id (concat "* PLAN " id) id)))
    (cl-letf (((symbol-function 'org-glance-view:fill-frame) #'ignore))
      (org-glance-test:with-overview (buffer graph nil)
        (goto-char (car (org-glance-overview--heading-region "two")))
        (org-glance-config-test--publish graph
         "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(q)\",\"WORK(w)\"]}]}")
        (org-glance-overview:refresh)
        (should (equal "two" (org-glance-overview:id-at-point)))
        (should (equal "PLAN" (org-get-todo-state)))
        (should (member "WORK" org-not-done-keywords))
        (should (equal ?q (cdr (assoc "PLAN" org-todo-key-alist))))))))

(ert-deftest org-glance-config-test-overview-is-a-standalone-agenda-source ()
  (org-glance-test:with-graph graph
    (org-glance-config-test--publish graph
     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"],\"inactive\":[\"FINISHED\"]}]}")
    (org-glance-config-test--snapshot graph "agenda-row"
      (org-glance-test:org-with-id "* PLAN Agenda task\nSCHEDULED: <2026-06-07 Sun>" "agenda-row"))
    (let ((file (org-glance-overview:write graph)))
      (unwind-protect
          (cl-letf (((symbol-function 'org-glance-graph:get-content)
                     (lambda (&rest _) (ert-fail "Agenda materialized a blob"))))
            (let ((org-agenda-skip-scheduled-if-done t))
              (should (cl-some (lambda (entry) (string-search "PLAN Agenda task" entry))
                               (org-agenda-get-day-entries file '(6 7 2026) :scheduled)))))
        (when-let* ((buffer (find-buffer-visiting file))) (org-glance--discard-buffer buffer))))))

(ert-deftest org-glance-config-test-overview-concurrent-publication-is-immutable ()
  (load (expand-file-name "test/fixtures/overview-export-peer.el" default-directory) nil t)
  (cl-letf (((symbol-function 'org-glance-cache--inventory-tick) #'ignore))
    (dolist (filter '(nil (:tags ("book"))))
      (dolist (kind '(presentation family))
        (dolist (serial '(nil t))
          (ert-info ((format "filter=%S kind=%S serial=%S" filter kind serial))
            (org-glance-test:with-graph graph
              (org-glance-config-test--publish graph
                                               "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}")
              (org-glance-config-test--snapshot graph "export-row"
                                                (org-glance-test:org-with-id "* PLAN Original :book:" "export-row"))
              (let* ((root (org-glance-graph:directory graph))
                     (held (org-glance-overview:cached-file graph filter))
                     (bytes (f-read-text held 'utf-8))
                     processes buffers)
                (unwind-protect
                    (progn
                      (dolist (role '(old new))
                        (when (eq role 'new)
                          (org-glance-test-overview-wait root "old-prepared")
                          (when serial (org-glance-test-overview-wait root "old-finished")))
                        (let ((buffer (generate-new-buffer " *overview-peer*")))
                          (push buffer buffers)
                          (push (make-process :name "overview-peer" :buffer buffer :noquery t
                                              :command (let ((load-path
                                                              (cons (expand-file-name "test/fixtures" default-directory)
                                                                    load-path)))
                                                         (org-glance--batch-worker-command
                                                          'org-glance-test-overview-peer
                                                          (list root (list 'quote role) (list 'quote filter)
                                                                (list 'quote kind) serial)))) processes)))
                      (let ((deadline (+ (float-time) 20)))
                        (while (cl-some #'process-live-p processes)
                          (when (> (float-time) deadline) (ert-fail "Export peers timed out"))
                          (accept-process-output nil 0.01)))
                      (cl-mapc (lambda (process buffer)
                                 (ert-info ((with-current-buffer buffer (buffer-string)))
                                   (should (= 0 (process-exit-status process))))) processes buffers)
                      (should (equal bytes (f-read-text held 'utf-8))))
                  (dolist (process processes) (when (process-live-p process) (delete-process process)))
                  (dolist (buffer buffers) (kill-buffer buffer)))))))))))

(ert-deftest org-glance-config-test-overview-interrupted-publisher-keeps-admitted-artifact ()
  (load (expand-file-name "test/fixtures/overview-export-peer.el" default-directory) nil t)
  (dolist (filter '(nil (:tags ("book"))))
    (dolist (kind '(presentation family))
      (org-glance-test:with-graph graph
        (org-glance-config-test--publish graph
         "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}")
        (org-glance-config-test--snapshot graph "export-row"
          (org-glance-test:org-with-id "* PLAN Original :book:" "export-row"))
        (let* ((held (org-glance-overview:cached-file graph filter))
               (bytes (f-read-text held 'utf-8))
               (spec (org-glance-overview--spec-sidecar held))
               (receipt (f-read-text spec 'utf-8))
               (write (symbol-function 'org-glance--atomic-write)))
          (org-glance-test-overview-change graph kind)
          (cl-letf (((symbol-function 'org-glance--atomic-write)
                     (lambda (path text &rest arguments)
                       (when (equal path spec) (error "Interrupted before manifest publication"))
                       (apply write path text arguments))))
            (should-error (org-glance-overview:write graph filter)))
          (should (equal receipt (f-read-text spec 'utf-8)))
          (should (equal bytes (f-read-text held 'utf-8)))
          (should-not (org-glance-overview--spec-owns-cache? filter held graph))
          (let* ((file (org-glance-overview:cached-file graph filter))
                 (text (f-read-text file 'utf-8)))
            (should (string-search (if (eq kind 'presentation) "PLAN(q)" "Updated") text))
            (cl-letf (((symbol-function 'org-glance-cache:overview-rows)
                       (lambda (&rest _) (ert-fail "Recovery admission fetched Rows"))))
              (should (equal file (org-glance-overview:cached-file graph filter))))))))))

(ert-deftest org-glance-config-test-overview-killed-publisher-cannot-change-held-export ()
  (load (expand-file-name "test/fixtures/overview-export-peer.el" default-directory) nil t)
  (cl-letf (((symbol-function 'org-glance-cache--inventory-tick) #'ignore))
    (dolist (filter '(nil (:tags ("book"))))
      (dolist (kind '(presentation family))
	(org-glance-test:with-graph graph
				    (org-glance-config-test--publish graph
								     "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}")
				    (org-glance-config-test--snapshot graph "export-row"
								      (org-glance-test:org-with-id "* PLAN Original :book:" "export-row"))
				    (let* ((root (org-glance-graph:directory graph))
					   (held (org-glance-overview:cached-file graph filter))
					   (bytes (f-read-text held 'utf-8))
					   (spec (org-glance-overview--spec-sidecar held))
					   (receipt (f-read-text spec 'utf-8))
					   (buffer (generate-new-buffer " *interrupted-overview*"))
					   (load-path (cons (expand-file-name "test/fixtures" default-directory) load-path))
					   (process (make-process :name "interrupted-overview" :buffer buffer :noquery t
								  :command (org-glance--batch-worker-command
									    'org-glance-test-overview-interrupt-peer
									    (list root (list 'quote filter) (list 'quote kind))))))
				      (unwind-protect
					  (progn
					    (org-glance-test-overview-wait root "publisher-paused")
					    (delete-process process)
					    (should (equal receipt (f-read-text spec 'utf-8)))
					    (should (equal bytes (f-read-text held 'utf-8)))
					    (org-glance-config:refresh graph)
					    (let ((file (org-glance-overview:cached-file graph filter)))
					      (should (string-search (if (eq kind 'presentation) "PLAN(q)" "Updated")
								     (f-read-text file 'utf-8)))
					      (should (equal bytes (f-read-text held 'utf-8)))))
					(when (process-live-p process) (delete-process process))
					(kill-buffer buffer))))))))

(ert-deftest org-glance-config-test-shared-settings-vectors ()
  (let ((path (getenv "GLANCE_CONFIG_SETTINGS_VECTORS")))
    (unless path (ert-skip "Set GLANCE_CONFIG_SETTINGS_VECTORS for paired typed fixtures"))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (entry (append (json-parse-buffer :null-object :null :false-object :false) nil))
        (ert-info ((gethash "name" entry))
          (let ((snapshot (org-glance-config-file:decode
                           (org-glance-config-file--serialize (gethash "config" entry)))))
            (if (eq t (gethash "invalid" entry))
                (should-error (org-glance-config:decode snapshot) :type 'org-glance-config-error)
              (let* ((settings (org-glance-config:decode snapshot))
                     (body (gethash "source" entry))
                     (tags (append (gethash "tags" entry) nil))
                     (presentation (org-glance-config:present settings tags body)))
                (should (equal (gethash "header" entry) (org-glance-org-presentation:header presentation)))
                (with-temp-buffer
                  (org-glance-config:insert-presentation presentation)
                  (should (equal (gethash "active" entry) (vconcat (delete-dups org-not-done-keywords))))
                  (should (equal (gethash "inactive" entry) (vconcat (delete-dups org-done-keywords))))
                  (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
                (should (= 4 (length (org-glance-config:fingerprints settings))))))))))))

(ert-deftest org-glance-config-test-read-only-prefix-and-span-boundary ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN(p)\"]}]}"))
         (body "* PLAN 文🙂\r\nBody\r\n")
         (presentation (org-glance-config:present settings nil (concat (make-string 37 ?\s) body)
                                                 :start 37))
         (offset (length (org-glance-org-presentation:prefix presentation))))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string))))
      (should (member "PLAN" org-not-done-keywords))
      (should-not (member "PLAN" org-done-keywords))
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (goto-char (point-min))
      (should-error (insert "bad") :type 'text-read-only)
      (goto-char (+ (point-min) offset))
      (let ((org-inhibit-logging t)) (org-todo "DONE"))
      (should (string-prefix-p "* DONE" (org-glance-config:canonical-body presentation (buffer-string)))))
    (should (equal '(39 43 "DONE") (org-glance-config:map-edit presentation
                                                               :start (+ offset 2) :end (+ offset 6) :text "DONE")))
    (should-error (org-glance-config:map-edit presentation :start 0 :end offset :text "")
                  :type 'org-glance-config-error)
    (should-error (org-glance-config:canonical-body presentation (concat "bad" body))
                  :type 'org-glance-config-error)))

(ert-deftest org-glance-config-test-source-headers-remain-owned-and-refreshable ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
         (body "#+TODO: PLAN | END\n#+PROPERTY: Owner 文\n* PLAN Title\n")
         (presentation (org-glance-config:present settings nil body)))
    (should (equal "" (org-glance-org-presentation:prefix presentation)))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal body (buffer-string)))
      (org-set-regexps-and-options)
      (should (member "PLAN" org-not-done-keywords))
      (should-not (member "PLAN" org-done-keywords))
      (goto-char (point-max)) (insert "Authored\n")
      (should (equal (concat body "Authored\n")
                     (org-glance-config:canonical-body presentation (buffer-string)))))))

(ert-deftest org-glance-config-test-decoding-and-fingerprints-observe-no-files ()
  (cl-letf (((symbol-function 'directory-files) (lambda (&rest _) (ert-fail "Settings enumerated a directory")))
            ((symbol-function 'insert-file-contents) (lambda (&rest _) (ert-fail "Settings read a file")))
            ((symbol-function 'insert-file-contents-literally) (lambda (&rest _) (ert-fail "Settings read raw bytes"))))
    (let* ((a (org-glance-config-test--settings "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
           (b (org-glance-config-test--settings " {\"clients\":{\"glance\":{\"theme\":\"dark\"}},\"todoCycles\":[{\"active\":[\"PLAN\"]}],\"format\":1}\n"))
           (c (org-glance-config-test--settings "{\"format\":1,\"todoCycles\":[{\"inactive\":[\"PLAN\"]}]}")))
      (should (equal (org-glance-config:fingerprints a) (org-glance-config:fingerprints b)))
      (should (equal (car (org-glance-config:fingerprints a)) (car (org-glance-config:fingerprints c))))
      (should-not (equal (cadr (org-glance-config:fingerprints a)) (cadr (org-glance-config:fingerprints c)))))))

(ert-deftest org-glance-config-test-fingerprints-preserve-the-settings-object ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"tags\":{\"book\":{\"todoCycles\":[{\"active\":[\"READING\"],\"inactive\":[\"READ\"]}]}}}"))
         (before (copy-tree (org-glance-config:keywords settings nil '("book"))))
         (fingerprints (org-glance-config:fingerprints settings)))
    (dotimes (_ 3)
      (should (equal fingerprints (org-glance-config:fingerprints settings)))
      (should (equal before (org-glance-config:keywords settings nil '("book")))))))

(ert-deftest org-glance-config-test-unknown-fields-are-diagnosed-and-retained ()
  (let* ((snapshot (org-glance-config-file:decode
                    "{\"format\":1,\"futureSetting\":{\"keep\":true},\"clients\":{\"other\":{\"value\":17}}}"))
         (before (org-glance-config-file--serialize (org-glance-config-file:object snapshot)))
         (settings (org-glance-config:decode snapshot)))
    (should (equal '("unknown setting: futureSetting") (org-glance-settings:diagnostics settings)))
    (org-glance-config:fingerprints settings)
    (should (equal before (org-glance-config-file--serialize (org-glance-config-file:object snapshot))))))

(ert-deftest org-glance-config-test-overview-refuses-incompatible-scopes ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"tags\":{\"a\":{\"todoCycles\":[{\"active\":[\"WAIT\"]}]},\"b\":{\"todoCycles\":[{\"inactive\":[\"WAIT\"]}]}}}"))
         (body "* WAIT Title\n")
         (a (org-glance-config:present settings '("a") body))
         (b (org-glance-config:present settings '("b") body)))
    (should (equal (org-glance-org-presentation:header a)
                   (org-glance-config:compatible-header (list (cons "a.org#1" a) (cons "a.org#2" a)))))
    (let ((error (should-error
                  (org-glance-config:compatible-header (list (cons "a.org#1" a) (cons "b.org#2" b)))
                  :type 'org-glance-config-error)))
      (should (string-match-p "a.org#1 and b.org#2" (error-message-string error))))))

(ert-deftest org-glance-config-test-file-context-outside-the-canonical-body ()
  (let* ((settings (org-glance-config-test--settings
                    "{\"format\":1,\"todoCycles\":[{\"active\":[\"PLAN\"]}]}"))
         (before "#+TODO: PLAN(p) | END(e)\r\n#+PROPERTY: Owner 文\r\n")
         (body "* PLAN Title文\r\n")
         (after "* DONE Sibling\r\n")
         (presentation (org-glance-config:present settings nil (concat before body after)
                         :start (length before) :end (+ (length before) (length body)))))
    (should (equal "#+TODO: TODO PLAN(p) | DONE END(e)\n"
                   (org-glance-org-presentation:header presentation)))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
    (should (equal (list (length before) (+ (length before) 1) "!")
                   (org-glance-config:map-edit presentation
                    :start (length (org-glance-org-presentation:prefix presentation))
                    :end (1+ (length (org-glance-org-presentation:prefix presentation))) :text "!")))))

(ert-deftest org-glance-config-test-unicode-identity-is-context-free ()
  (dolist (pair '(("ΑΣ" . "ασ") ("BØK" . "bøk") ("I" . "i")))
    (let* ((decode (lambda (name)
                     (org-glance-config-test--settings
                      (format "{\"format\":1,\"tree\":{\"inboxTag\":\"%s\"},\"tags\":{\"%s\":{\"storage\":\"local\",\"todoCycles\":[{\"inactive\":[\"PLAN\"]}]}}}"
                              name name))))
           (upper (funcall decode (car pair)))
           (lower (funcall decode (cdr pair))))
      (should (equal (org-glance-config:fingerprints upper) (org-glance-config:fingerprints lower)))
      (should (equal (org-glance-config:keywords upper nil (list (car pair)))
                     (org-glance-config:keywords lower nil (list (cdr pair)))))))
  (should (equal "ας" (org-glance-config--tag-name "ας")))
  (should (equal "i\u0307" (org-glance-config--tag-name "İ")))
  (should-not (org-glance-config--tag (org-glance-config--tag-name "İ"))))

(ert-deftest org-glance-config-test-interleaved-aliases-share-one-source-observation ()
  (let* ((settings (org-glance-config-test--settings "{\"format\":1}"))
         (before "#+TODO: FIRST | END\r\n#+SEQ_TODO: PLAN(p) | FINISHED\r\n#+TODO: PLAN(q) LAST | STOP\r\n")
         (body "* PLAN Title文\r\n")
         (source (concat before body))
         (observe (symbol-function 'org-glance-tag-config:todo-declarations))
         (calls 0)
         (presentation
          (cl-letf (((symbol-function 'org-glance-tag-config:todo-declarations)
                     (lambda (text) (cl-incf calls) (funcall observe text))))
            (org-glance-config:present settings nil source :start (length before)))))
    (should (equal "#+TODO: TODO FIRST PLAN(p) LAST | DONE END FINISHED STOP\n"
                   (org-glance-org-presentation:header presentation)))
    (should (= 1 calls))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
    (let ((offset (length (org-glance-org-presentation:prefix presentation))))
      (should (equal (list (+ (length before) 2) (+ (length before) 6) "DONE")
                     (org-glance-config:map-edit presentation :start (+ offset 2) :end (+ offset 6) :text "DONE"))))))

(ert-deftest org-glance-config-test-ordered-declarations-exclude-opaque-blocks ()
  (let* ((source "#+TODO: FIRST | END\n#+begin_src org\n#+SEQ_TODO: FAKE(f) | FALSE\n#+end_src\n#+TYP_TODO: PLAN(p) | STOP\n#+TODO: PLAN(q) | CLOSED\n")
         (declarations (org-glance-tag-config:todo-declarations source)))
    (should (equal '("FIRST | END" "PLAN(p) | STOP" "PLAN(q) | CLOSED")
                   (mapcar (lambda (entry) (nth 2 entry)) declarations)))
    (should (equal '(("FIRST" "PLAN") "END" "STOP" "CLOSED")
                   (org-glance-tag-config:declaration-keywords declarations)))
    (dolist (entry declarations)
      (should (string-prefix-p "#+" (substring source (car entry) (cadr entry)))))))

(ert-deftest org-glance-config-test-unicode-admission-uses-parser-classes ()
  (should-error (org-glance-config--token "Ⅷ") :type 'org-glance-config-error)
  (should (equal "PLAN" (org-glance-config--token "PLAN(\u200b)")))
  (should-error (org-glance-config-test--settings
                 "{\"format\":1,\"tree\":{\"repositoryRemote\":\"\u200borigin\u200b\"}}")
                :type 'org-glance-config-error))

(ert-deftest org-glance-config-test-crlf-block-observation-keeps-original-spans ()
  (let* ((before "# Author 文\r\n#+BEGIN_EXAMPLE\r\n#+TODO: FAKE(f) | FALSE\r\n#+END_EXAMPLE\r\n#+TODO: PLAN(p) | END(e)\n")
         (body "* PLAN Title文\r\n")
         (source (concat before body))
         (declarations (org-glance-tag-config:todo-declarations source))
         (start (string-match (regexp-quote "#+TODO: PLAN") source))
         (settings (org-glance-config-test--settings "{\"format\":1}"))
         (presentation (org-glance-config:present settings nil source :start (length before))))
    (should (equal (list (list start (length before) "PLAN(p) | END(e)")) declarations))
    (with-temp-buffer
      (org-glance-config:insert-presentation presentation)
      (should (equal '("PLAN" . 112) (assoc "PLAN" org-todo-key-alist)))
      (should (equal body (org-glance-config:canonical-body presentation (buffer-string)))))
    (let ((offset (length (org-glance-org-presentation:prefix presentation))))
      (should (equal (list (+ (length before) 2) (+ (length before) 6) "DONE")
                     (org-glance-config:map-edit presentation :start (+ offset 2) :end (+ offset 6) :text "DONE"))))))

(ert-deftest org-glance-config-test-crlf-observations-and-presentations-are-pure ()
  (let* ((settings (org-glance-config-test--settings "{\"format\":1}"))
         (a "# Author\r\n#+TODO: PLAN(p) | END(e)\r\n* PLAN Title\r\n")
         (b "# 文🙂 longer preamble\n#+TODO: PLAN(p) | END(e)\r\n* PLAN Different文\r\n"))
    (dolist (source (list a a a b a b b a))
      (ert-info ((format "source %S" source))
        (let* ((start (string-match (regexp-quote "#+TODO:") source))
               (end (string-match "\\* PLAN" source))
               (expected (list (list start end "PLAN(p) | END(e)")))
               (observe (symbol-function 'org-glance-tag-config:todo-declarations))
               (calls 0))
          (should (equal expected (org-glance-tag-config:todo-declarations source)))
          (should (equal "#+TODO: PLAN(p) | END(e)\r\n" (substring source start end)))
          (cl-letf (((symbol-function 'org-glance-tag-config:todo-declarations)
                     (lambda (text) (cl-incf calls) (funcall observe text))))
            (let* ((p (org-glance-config:present settings nil source :start end))
                   (prefix (org-glance-org-presentation:prefix p)))
              (should (= calls 1))
              (should (equal "#+TODO: TODO PLAN(p) | DONE END(e)\r\n" prefix))
              (should (equal (substring source end)
                             (org-glance-config:canonical-body
                              p (concat prefix (org-glance-org-presentation:body p)))))
              (should (equal (list (+ end 2) (+ end 6) "DONE")
                             (org-glance-config:map-edit p :start (+ (length prefix) 2)
                                                        :end (+ (length prefix) 6) :text "DONE"))))))))))

(provide 'test-config)
;;; test-config.el ends here
