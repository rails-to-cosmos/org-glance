;;; test-material-virtual.el --- Virtual material lifecycle -*- lexical-binding: t; -*-

(require 'test-helpers)

(ert-deftest org-glance-test:virtual-save-preserves-editor-state ()
  "Local publication preserves the editor, location, folds and authored undo."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "stable" "* Stable" "** Child" "body"))
    (org-glance-test:with-material (buffer graph "stable")
      (let* ((old-base org-glance-material--base)
             (old-bytes (f-read-text (plist-get old-base :path) 'utf-8-unix)))
        (goto-char (point-min))
        (org-fold-hide-subtree)
        (goto-char (point-max))
        (undo-boundary)
        (insert " edited")
        (undo-boundary)
        (set-mark (1- (point)))
        (narrow-to-region 3 (point-max))
        (let ((position (point)) (mark-position (mark))
              (minimum (point-min)) (undo buffer-undo-list)
              (adopt (symbol-function 'org-glance-material--adopt))
              (read-text (symbol-function 'f-read-text)))
          (cl-letf (((symbol-function 'org-glance-material--adopt)
                     (lambda (receipt)
                       (cl-letf (((symbol-function 'f-read-text)
                                  (lambda (path &rest args)
                                    (when (org-glance-version:file-p path)
                                      (ert-fail "Save adoption reloaded a snapshot"))
                                    (apply read-text path args)))
                                 ((symbol-function 'revert-buffer)
                                  (lambda (&rest _) (ert-fail "Save reverted the editor")))
                                 ((symbol-function 'find-file-noselect)
                                  (lambda (&rest _) (ert-fail "Save visited a snapshot"))))
                         (funcall adopt receipt)))))
            (let ((receipt (org-glance-material:save)))
              (should (org-glance-publication-p receipt))
              (should (equal (org-glance-version:id
                              (org-glance-publication:version receipt))
                             (plist-get org-glance-material--base :version)))))
          (should-not buffer-file-name)
          (should (= position (point)))
          (should (= mark-position (mark)))
          (should (= minimum (point-min)))
          (should (eq undo buffer-undo-list))
          (should-not (buffer-modified-p))
          (widen)
          (should (org-fold-folded-p (save-excursion (goto-char 1) (line-end-position))))
          (let ((new-base org-glance-material--base))
            (undo-only 1)
            (should (buffer-modified-p))
            (should (eq new-base org-glance-material--base))
            (should-not (string-match-p " edited" (buffer-string)))))
        (should (equal old-bytes (f-read-text (plist-get old-base :path) 'utf-8-unix)))))))

(ert-deftest org-glance-test:virtual-registry-is-logical-and-warm ()
  "Aliases reuse a dirty editor without storage reads; equal titles stay distinct."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Same")
                          (org-glance-test:headline "b" "* Same"))
    (org-glance-test:with-material (buffer graph "a")
      (org-glance-test:with-material (other graph "b")
        (should-not (eq buffer other)))
      (insert "dirty ")
      (with-temp-directory alias-root
        (let ((alias (expand-file-name "alias" alias-root)))
          (make-symbolic-link (org-glance-graph:directory graph) alias)
          (let ((same (org-glance-graph alias)))
            (should (eq same graph))
            (cl-letf (((symbol-function 'org-glance-cache:logical-id)
                       (lambda (&rest _) (ert-fail "Warm lookup queried storage")))
                      ((symbol-function 'org-glance-version:observe)
                       (lambda (&rest _) (ert-fail "Warm lookup observed a family"))))
              (should (eq buffer (org-glance-material:open same "a"))))))))
    (should-not (org-glance-material:buffer graph "a"))))

(ert-deftest org-glance-test:virtual-save-completes-the-same-event-after-faults ()
  "Failures after rename, WAL append or projection leave one retryable child."
  (dolist (fault '(rename wal projection committed))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "retry" "* Retry"))
      (org-glance-test:with-material (buffer graph "retry")
        (goto-char (point-max)) (insert "\nSaved draft\n")
        (let ((base org-glance-material--base)
              (families org-glance-graph-after-family-append-functions)
              (apply-family (symbol-function 'org-glance-cache--apply-family)))
          (let ((org-glance-version--publication-hook
                 (lambda (phase)
                   (when (and (eq fault 'rename) (eq phase 'after-final-rename))
                     (error "Injected rename failure"))))
                (org-glance-graph--publication-hook
                 (lambda (phase)
                   (when (and (eq fault 'committed) (eq phase 'after-sqlite-commit))
                     (error "Injected completion failure"))))
                (org-glance-graph-after-family-append-functions
                 (if (eq fault 'wal)
                     (list (lambda (&rest _) (error "Injected WAL failure")))
                   families)))
            (cl-letf (((symbol-function 'org-glance-cache--apply-family)
                       (lambda (&rest args)
                         (when (eq fault 'projection) (error "Injected projection failure"))
                         (apply apply-family args))))
              (should-error (org-glance-material:save))))
          (should (eq base org-glance-material--base))
          (should (buffer-modified-p))
          (should org-glance-material--pending)
          (let* ((receipt org-glance-material--pending)
                 (version (org-glance-publication:version receipt))
                 (event (org-glance-version:id version)))
            (should (file-directory-p (org-glance-version:directory version)))
            (should (eq receipt (org-glance-material:save)))
            (should-not org-glance-material--pending)
            (should-not (buffer-modified-p))
            (should (equal event (plist-get org-glance-material--base :version)))
            (should (= 2 (length (org-glance-version:inventory
                                 (org-glance-graph:headline-data-path graph "retry")))))
            (should (equal "Retry" (org-glance-headline-metadata:title
                                   (org-glance-cache:headline graph "retry"))))))))))

(ert-deftest org-glance-test:virtual-recovery-keeps-newer-edits ()
  "Recovery finishes the recorded child before publishing the newer draft."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "retry" "* Retry"))
    (org-glance-test:with-material (buffer graph "retry")
      (goto-char (point-max)) (insert "\nFirst draft\n")
      (let ((org-glance-version--publication-hook
             (lambda (phase) (when (eq phase 'after-final-rename) (error "Injected")))))
        (should-error (org-glance-material:save)))
      (let* ((first (org-glance-publication:version org-glance-material--pending))
             (first-id (org-glance-version:id first)))
        (insert "Newer edit\n")
        (let ((second (org-glance-publication:version (org-glance-material:save))))
          (should (equal (list first-id) (org-glance-version:parents second)))
          (should (string-match-p "Newer edit" (buffer-string)))
          (should-not (string-match-p "Newer edit"
                                      (f-read-text (f-join (org-glance-version:directory first)
                                                          "data.org") 'utf-8-unix)))
          (should (= 3 (length (org-glance-version:inventory
                               (org-glance-graph:headline-data-path graph "retry"))))))))))

(ert-deftest org-glance-test:virtual-failure-before-publication-and-quit ()
  "A pre-publication failure releases pending state; a post-rename quit retains it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nEdit")
      (let ((org-glance-version--publication-hook
             (lambda (phase) (when (eq phase 'before-temp-write) (error "Injected")))))
        (should-error (org-glance-material:save)))
      (should-not org-glance-material--pending)
      (should (buffer-modified-p))
      (let ((org-glance-version--publication-hook
             (lambda (phase) (when (eq phase 'after-final-rename) (signal 'quit nil)))))
        (should (eq 'quit (condition-case nil (org-glance-material:save) (quit 'quit)))))
      (should org-glance-material--pending)
      (org-glance-material:save)
      (should-not (buffer-modified-p)))))

(ert-deftest org-glance-test:virtual-stale-frontier-preserves-draft ()
  "A writer before validation refuses the save without publishing or losing edits."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Original"))
    (org-glance-test:with-material (buffer graph "a")
      (let ((base org-glance-material--base))
        (goto-char (point-max)) (insert "\nMy draft")
        (org-glance-graph:add graph (org-glance-test:headline "a" "* External"))
        (should (eq buffer (org-glance-material:open graph "a")))
        (org-glance-view--refresh-when-stale)
        (should (string-match-p "My draft" (buffer-string)))
        (should-error (org-glance-material:save) :type 'user-error)
        (should-not org-glance-material--pending)
        (should (eq base org-glance-material--base))
        (should (= 2 (length (org-glance-version:inventory
                             (org-glance-graph:headline-data-path graph "a")))))
        (should (buffer-modified-p))))))

(ert-deftest org-glance-test:virtual-concurrency-keeps-captured-parents ()
  "A racing child stays concurrent and blocks subsequent ordinary publication."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (let* ((parents (plist-get org-glance-material--base :frontier))
             (dir (org-glance-graph:headline-data-path graph "a"))
             (org-glance-version--publication-hook
              (lambda (phase)
                (when (eq phase 'before-temp-write)
                  (let ((org-glance-version--publication-hook nil))
                    (org-glance-version:write-snapshot
                     dir "a" parents "other"
                     (org-glance-test:org-with-id "* Racing" "a")))))))
        (goto-char (point-max)) (insert "\nLocal edit")
        (let* ((receipt (org-glance-material:save))
               (version (org-glance-publication:version receipt)))
          (should (equal parents (org-glance-version:parents version)))
          (should (= 2 (length (org-glance-publication:frontier receipt))))))
      (should (plist-get org-glance-material--base :reconcile))
      (insert " newer")
      (should-error (org-glance-material:save) :type 'user-error))))

(ert-deftest org-glance-test:virtual-refresh-pulls-clean-content-and-retains-dangerous-state ()
  "Clean refresh loads a newer source; deletion preserves the visible content."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Original"))
    (org-glance-test:with-material (buffer graph "a")
      (org-glance-graph:add graph (org-glance-test:headline "a" "* External"))
      (should (string-match-p "Original" (buffer-string)))
      (org-glance-view--refresh-when-stale)
      (should (string-match-p "External" (buffer-string)))
      (should-not buffer-undo-list)
      (should-not (buffer-modified-p))
      (org-glance-graph:delete graph "a")
      (should (eq buffer (org-glance-material:open graph "a")))
      (should-error (org-glance-material:revert) :type 'user-error)
      (should (string-match-p "External" (buffer-string)))
      (goto-char (point-max)) (insert "\ndraft")
      (should-error (org-glance-material:save) :type 'user-error))))

(ert-deftest org-glance-test:virtual-save-observes-edits-during-publication ()
  "An edit during publication remains dirty against the exact saved base."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nSaved")
      (let ((org-glance-graph--publication-hook
             (lambda (phase)
               (when (eq phase 'before-live-frame-publication)
                 (with-current-buffer buffer
                   (goto-char (point-max)) (insert " newer"))))))
        (org-glance-material:save))
      (should (buffer-modified-p))
      (should (string-match-p "newer" (buffer-string)))
      (should-not (string-match-p "newer"
                                  (f-read-text (plist-get org-glance-material--base :path)
                                               'utf-8-unix))))))

(ert-deftest org-glance-test:virtual-native-save-all-and-close ()
  "Native save-all respects predicates, and killing a draft supports all choices."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((save-some-buffers-default-predicate nil))
        (save-some-buffers t (lambda () nil))
        (should (buffer-modified-p))
        (save-some-buffers t)
        (should-not (buffer-modified-p)))
      (should-not buffer-file-name)
      (insert "dirty ")
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?c)))
        (should-not (kill-buffer buffer)))
      (should (buffer-live-p buffer))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s)))
        (should (kill-buffer buffer)))
      (should-not (buffer-live-p buffer)))
    (should-not (org-glance-material:buffer graph "a"))))

(ert-deftest org-glance-test:virtual-source-edits-save-through-the-logical-editor ()
  "Org source editing updates the virtual parent and uses its publication path."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* A" "#+begin_src emacs-lisp" "(+ 1 2)" "#+end_src"))
    (org-glance-test:with-material (buffer graph "a")
      (save-window-excursion
        (switch-to-buffer buffer)
        (search-forward "(+ 1 2)")
        (org-edit-special)
        (should-not buffer-file-name)
        (should-not buffer-auto-save-file-name)
        (should backup-inhibited)
        (should-not create-lockfiles)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (search-forward "2") (replace-match "3")
              (org-edit-src-exit))
          (when (bound-and-true-p org-src-mode) (org-edit-src-abort))))
      (should (string-match-p (regexp-quote "(+ 1 3)") (buffer-string)))
      (should (buffer-modified-p))
      (org-glance-material:save)
      (should-not buffer-file-name)
      (should (string-match-p (regexp-quote "(+ 1 3)") (org-glance-graph:get-content graph "a"))))))

(ert-deftest org-glance-test:virtual-preview-and-restore-are-explicit ()
  "Previews stay pinned; restore preserves identity and protects dirty drafts."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Original"))
    (org-glance-test:with-material (buffer graph "a")
      (let* ((version (plist-get org-glance-material--base :version))
             (preview (org-glance-material:preview graph "a" version)))
        (unwind-protect
            (progn
              (org-glance-material:set-title graph "a" "Changed")
              (should (eq buffer (org-glance-material:buffer graph "a")))
              (with-current-buffer preview
                (should buffer-read-only)
                (should (string-match-p "Original" (buffer-string))))
              (insert "draft ")
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (should-error (org-glance-material:restore graph "a" version) :type 'user-error))
              (should (string-match-p "draft" (buffer-string)))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (should (eq buffer (org-glance-material:restore graph "a" version))))
              (let ((parents (plist-get org-glance-material--base :frontier)))
                (should (equal version (plist-get org-glance-material--base :version)))
                (let ((saved (org-glance-publication:version (org-glance-material:save))))
                  (should (equal parents (org-glance-version:parents saved)))
                  (should-not (equal version (org-glance-version:id saved))))))
          (kill-buffer preview))))))

(ert-deftest org-glance-test:virtual-relation-entries-use-ordinary-undo ()
  "Relation insertion records a normal edit while preserving other draft edits."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A" "body")
                          (org-glance-test:headline "b" "* B"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (undo-boundary) (insert " edited") (undo-boundary)
      (let ((text (buffer-string))
            (revision org-glance-material--revision) (position (copy-marker (point))))
        (org-glance-graph:add graph (org-glance-test:headline "b" "* B" "[[glance:a][A]]"))
        (narrow-to-region 3 (point-max))
        (org-glance-material--insert-relation-entries)
        (undo-boundary)
        (should (= 3 (point-min)))
        (should (= position (point)))
        (should (< revision org-glance-material--revision))
        (widen)
        (should (buffer-modified-p))
        (should (string-match-p "Referred by" (buffer-string)))
        (undo-only 1)
        (should (equal text (buffer-string)))
        (let ((last-command 'undo)) (undo-only 1))
        (should-not (string-match-p " edited" (buffer-string)))
        (set-marker position nil)))))


(ert-deftest org-glance-test:virtual-native-exit-preserves-unsaved-drafts ()
  "Native exit saves virtual editors and requires consent for skipped drafts."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((confirm-kill-emacs nil) (confirm-kill-processes nil)
            (use-dialog-box nil)
            (kill-emacs-query-functions (list #'org-glance-material--exit-query))
            exited)
        (cl-letf (((symbol-function 'kill-emacs) (lambda (&rest _) (setq exited t))))
          (save-buffers-kill-emacs t)
          (should exited)
          (should-not (buffer-modified-p))
          (insert " newer")
          (setq exited nil)
          (cl-letf (((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (save-buffers-kill-emacs t))
          (should-not exited)
          (should (buffer-modified-p))
          (cl-letf (((symbol-function 'save-some-buffers) (lambda (&rest _) nil))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (save-buffers-kill-emacs t))
          (should exited)
          (should (buffer-modified-p)))))))

(ert-deftest org-glance-test:virtual-close-save-failure-keeps-the-draft ()
  "A failed close save preserves the buffer; explicit discard unregisters it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s))
                ((symbol-function 'org-glance-graph:publish-edit)
                 (lambda (&rest _) (error "Injected save failure"))))
        (should-error (kill-buffer buffer)))
      (should (buffer-live-p buffer))
      (should (buffer-modified-p))
      (should (string-match-p "Draft" (buffer-string)))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d)))
        (should (kill-buffer buffer)))
      (should-not (org-glance-material:buffer graph "a")))))

(ert-deftest org-glance-test:virtual-crypt-save-is-memory-only-and-presentation-stable ()
  "Crypt saves publish sealed bytes and preserve editor plaintext and undo."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt
       (org-glance-test:headline "a" "* Secret" "private payload") "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "a" :decrypt t)
        (goto-char (point-min)) (search-forward "private payload")
        (replace-match "unique private draft")
        (let ((draft (buffer-string)) (undo buffer-undo-list)
              (write (symbol-function 'write-region)))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (start end &rest args)
                       (let ((text (if (stringp start) start
                                     (buffer-substring-no-properties start end))))
                         (when (string-match-p "unique private draft" text)
                           (ert-fail "Plaintext reached a disk writer")))
                       (apply write start end args))))
            (org-glance-material:save))
          (should (equal draft (buffer-string)))
          (should (eq undo buffer-undo-list))
          (should-not (buffer-modified-p))
          (should-not buffer-auto-save-file-name)
          (should backup-inhibited)
          (should-not create-lockfiles)
          (should-not (string-match-p "unique private draft"
                                      (org-glance-graph:get-content graph "a"))))))))

(ert-deftest org-glance-test:virtual-encrypted-external-refresh-forgets-password ()
  "An external encrypted source returns to sealed presentation with no proof."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt (org-glance-test:headline "a" "* Secret" "old") "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "a" :decrypt t)
        (should org-glance-material--password-proof)
        (org-glance-graph:add graph
          (org-glance-headline:encrypt (org-glance-test:headline "a" "* Secret" "new") "other"))
        (org-glance-view--refresh-when-stale)
        (should (string-match-p "aes-encrypted" (buffer-string)))
        (should-not org-glance-material--password)
        (should-not org-glance-material--password-proof)
        (should-not (buffer-modified-p))))))

(ert-deftest org-glance-test:virtual-decrypt-failure-preserves-dirty-content ()
  "A failed explicit decrypt retains public edits and the logical editor."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt (org-glance-test:headline "a" "* Secret" "old") "pw"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-min)) (end-of-line) (insert " changed")
      (let ((draft (buffer-string)) (base org-glance-material--base))
        (org-glance-test:answering ((read-passwd "wrong"))
          (should-error (org-glance-material:decrypt) :type 'user-error))
        (should (buffer-live-p buffer))
        (should (eq base org-glance-material--base))
        (should (equal draft (buffer-string)))
        (should (buffer-modified-p))))))

(ert-deftest org-glance-test:virtual-after-save-hooks-and-recursive-save ()
  "Hooks run once; later edits stay dirty, and notification faults keep success."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((before 0) (after 0))
        (add-hook 'before-save-hook
                  (lambda ()
                    (cl-incf before)
                    (should-error (save-buffer) :type 'user-error)) nil t)
        (add-hook 'after-save-hook
                  (lambda () (cl-incf after) (insert "newer")) nil t)
        (org-glance-material:save)
        (should (= 1 before))
        (should (= 1 after))
        (should (buffer-modified-p))
        (setq-local before-save-hook nil
                    after-save-hook (list (lambda () (error "Injected notification failure"))))
        (should (org-glance-publication-p (org-glance-material:save)))
        (should-not org-glance-material--pending)
        (should-not (buffer-modified-p))
        (should-not (org-glance-material:save))))))

(ert-deftest org-glance-test:virtual-file-attachment-refused-and-export-independent ()
  "File attachment cannot publish; export retains the virtual editor's identity."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((path (expand-file-name "export.org" (org-glance-graph:directory graph)))
            (base org-glance-material--base))
        (let ((buffer-file-name path))
          (should-error (org-glance-material:save) :type 'user-error))
        (should-error (set-visited-file-name path t) :type 'user-error)
        (should-error (write-file path) :type 'user-error)
        (should-not buffer-file-name)
        (should-not buffer-auto-save-file-name)
        (should-not (file-exists-p path))
        (org-glance-material:export path)
        (should (string-match-p "Draft" (f-read-text path 'utf-8-unix)))
        (should-not buffer-file-name)
        (should (buffer-modified-p))
        (should (eq base org-glance-material--base))))))

(ert-deftest org-glance-test:virtual-save-family-work-contract ()
  "A save observes its family once before writing and once after mutation."
  (dolist (depth '(1 8 32))
    (org-glance-test:with-graph graph
      (dotimes (index depth)
        (org-glance-graph:add graph
          (org-glance-test:headline "a" (format "* History %d" index))))
      (org-glance-test:with-material (buffer graph "a")
        (let ((observations 0)
              (observe (symbol-function 'org-glance-version:observe)))
          (cl-letf (((symbol-function 'org-glance-version:observe)
                     (lambda (&rest args) (cl-incf observations) (apply observe args))))
            (dotimes (_ 10) (should (eq buffer (org-glance-material:open graph "a"))))
            (should (= 0 observations))
            (goto-char (point-max)) (insert "\nEdited")
            (org-glance-material:save)
            (should (= 2 observations))
            (setq observations 0)
            (org-glance-material:save)
            (should (= 0 observations))))))))


(ert-deftest org-glance-test:virtual-indirect-edits-save-the-logical-base ()
  "Indirect text edits advance the logical revision and save the base editor."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (let ((indirect (clone-indirect-buffer " *material-indirect*" nil))
            (revision org-glance-material--revision))
        (unwind-protect
            (progn
              (with-current-buffer indirect
                (goto-char (point-max)) (insert "\nIndirect edit"))
              (should (> org-glance-material--revision revision))
              (with-current-buffer indirect
                (should (org-glance-publication-p (org-glance-material:save))))
              (should-not (buffer-modified-p))
              (with-current-buffer indirect (insert " newer"))
              (kill-buffer indirect)
              (should (buffer-modified-p))
              (should (eq buffer (org-glance-material:buffer graph "a"))))
          (when (buffer-live-p indirect) (kill-buffer indirect)))))))

(ert-deftest org-glance-test:virtual-damaged-source-preserves-visible-draft ()
  "Missing or corrupt source payloads refuse publication and retain the draft."
  (dolist (damage '(missing mismatch))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "a" "* A" "body"))
      (org-glance-test:with-material (buffer graph "a")
        (let ((base org-glance-material--base))
          (goto-char (point-max)) (insert " draft")
          (if (eq damage 'missing) (delete-file (plist-get base :path))
            (f-write-text "corrupted" 'utf-8-unix (plist-get base :path)))
          (should-error (org-glance-material:save) :type 'user-error)
          (should-not org-glance-material--pending)
          (should (eq base org-glance-material--base))
          (should (buffer-modified-p))
          (should (string-match-p "body draft" (buffer-string))))))))

(ert-deftest org-glance-test:virtual-recovery-refuses-a-corrupted-receipt-source ()
  "Recovery verifies the durable child and retains pending work on corruption."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((org-glance-version--publication-hook
             (lambda (phase) (when (eq phase 'after-final-rename) (error "Injected")))))
        (should-error (org-glance-material:save)))
      (let* ((receipt org-glance-material--pending)
             (path (f-join (org-glance-version:directory
                            (org-glance-publication:version receipt)) "data.org")))
        (f-append-text "corrupted" 'utf-8-unix path)
        (should-error (org-glance-material:save))
        (should (eq receipt org-glance-material--pending))
        (should (buffer-modified-p))
        (should (= 2 (length (org-glance-version:inventory
                             (org-glance-graph:headline-data-path graph "a")))))))))

(ert-deftest org-glance-test:virtual-diff-keeps-the-editor-and-pinned-base ()
  "Diff compares separate buffers without moving the editor's source reference."
  (require 'diff)
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max)) (insert "\nDraft")
      (let ((base org-glance-material--base) preview)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'diff-buffers)
                         (lambda (old new &rest _)
                           (setq preview old)
                           (with-current-buffer old
                             (should buffer-read-only)
                             (should-not (string-match-p "Draft" (buffer-string))))
                           (with-current-buffer new
                             (should (string-match-p "Draft" (buffer-string))))
                           'diff-result)))
                (should (eq 'diff-result (diff-buffer-with-file buffer))))
              (should (eq base org-glance-material--base))
              (should-not buffer-file-name)
              (should (buffer-modified-p)))
          (when (buffer-live-p preview) (kill-buffer preview)))))))


(ert-deftest org-glance-test:virtual-crypt-markers-always-require-sealed-publication ()
  "Typed crypt markers seal on save; malformed or removed markers preserve drafts."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A" "public"))
    (org-glance-test:with-material (buffer graph "a")
      (goto-char (point-max))
      (insert "\n#+begin_crypt\nprivate typed block\n")
      (should-error (org-glance-material:save) :type 'user-error)
      (insert "#+end_crypt\n")
      (org-glance-test:answering ((read-passwd "pw"))
        (org-glance-material:save))
      (should (string-match-p "private typed block" (buffer-string)))
      (should-not (string-match-p "private typed block" (org-glance-graph:get-content graph "a")))
      (should (string-match-p "aes-encrypted" (org-glance-graph:get-content graph "a")))
      (let ((base org-glance-material--base))
        (goto-char (point-min))
        (while (re-search-forward "^#\\+\\(?:begin\\|end\\)_crypt\n" nil t)
          (replace-match ""))
        (should-error (org-glance-material:save) :type 'user-error)
        (should (eq base org-glance-material--base))
        (should (buffer-modified-p))))))

(ert-deftest org-glance-test:virtual-password-validation-survives-hook-demotion ()
  "A demoted before-save error cannot bypass the password validation gate."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt (org-glance-test:headline "a" "* Secret" "private") "right"))
    (org-glance-test:answering ((read-passwd "right"))
      (org-glance-test:with-material (buffer graph "a" :decrypt t)
        (goto-char (point-min)) (search-forward "private") (insert " edit")
        (org-glance-material--clear-password)
        (add-hook 'before-save-hook (lambda () (error "Injected ordinary hook error")) nil t)
        (let ((base org-glance-material--base))
          (let ((debug-on-error nil))
            (org-glance-test:answering ((read-passwd "wrong"))
              (should-error (org-glance-material:save) :type 'user-error)))
          (should (eq base org-glance-material--base))
          (should-not org-glance-material--pending)
          (should (= 1 (length (org-glance-version:inventory
                               (org-glance-graph:headline-data-path graph "a")))))
          (should (buffer-modified-p)))))))


(ert-deftest org-glance-test:virtual-legacy-validation-precedes-root-migration ()
  "A stale legacy source writes no event; a valid retry migrates then publishes."
  (org-glance-test:with-graph graph
    (let* ((headline (org-glance-test:headline "a" "* Legacy" "body"))
           (content (org-glance-headline:contents headline))
           (dir (org-glance-graph:headline-data-path graph "a"))
           (path (f-join dir "data.org")))
      (org-glance-test:write path content)
      (org-glance-graph:insert graph (list (org-glance-headline:metadata headline)))
      (org-glance-test:with-material (buffer graph "a")
        (goto-char (point-max)) (insert " draft")
        (f-write-text "corrupted" 'utf-8-unix path)
        (should-error (org-glance-material:save) :type 'user-error)
        (should-not (org-glance-version:inventory dir))
        (should (buffer-modified-p))
        (f-write-text content 'utf-8-unix path)
        (org-glance-material:save)
        (should (= 2 (length (org-glance-version:inventory dir))))
        (should-not (buffer-modified-p))
        (should-not buffer-file-name)))))


(ert-deftest org-glance-test:virtual-bulk-note-failure-retains-draft-and-continues ()
  "A deferred note save failure retains its receipt and advances the bulk queue."
  (org-glance-test:with-note-origin (origin)
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "n1" "* TODO A")
                            (org-glance-test:headline "n2" "* TODO B"))
      (let ((org-todo-keywords '((sequence "TODO" "DONE")))
            (org-log-done 'note) (org-log-into-drawer t)
            (this-command 'org-glance-test-bulk)
            (finalized 'unset)
            (org-glance-version--publication-hook
             (lambda (phase)
               (when (and (equal "n1" org-glance-material--id)
                          (eq phase 'after-final-rename))
                 (error "Injected first-row publication failure")))))
        (unwind-protect
            (progn
              (with-current-buffer origin
                (org-glance-material:set-todo-bulk
                 graph '("n1" "n2") "DONE"
                 (lambda (changed skipped) (setq finalized (list changed skipped)))))
              (with-timeout (5 (ert-fail "Bulk note queue stopped after a failed row"))
                (while (eq finalized 'unset)
                  (let ((note (get-buffer "*Org Note*")))
                    (if (not (buffer-live-p note)) (sit-for 0.02)
                      (with-current-buffer note
                        (insert "reason")
                        (let ((org-note-abort nil)) (funcall org-finish-function)))))))
              (should (equal '("n2") (car finalized)))
              (should (equal "n1" (caar (cadr finalized))))
              (let ((failed (org-glance-material:buffer graph "n1")))
                (should (buffer-live-p failed))
                (should (buffer-modified-p failed))
                (with-current-buffer failed
                  (should org-glance-material--pending)
                  (should (string-match-p "reason" (buffer-string))))))
          (when-let* ((failed (org-glance-material:buffer graph "n1")))
            (org-glance--discard-buffer failed)))))))


(ert-deftest org-glance-test:virtual-transform-preserves-decrypted-presentation ()
  "Ordinary transforms retain decrypted text, credentials and editor identity."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt (org-glance-test:headline "a" "* Secret" "private body") "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "a" :decrypt t)
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _) (ert-fail "Ordinary transform forgot the password"))))
          (org-glance-material:set-title graph "a" "Renamed"))
        (should (eq buffer (org-glance-material:buffer graph "a")))
        (should (string-match-p "private body" (buffer-string)))
        (should (string-match-p "Renamed" (buffer-string)))
        (should org-glance-material--encrypted)
        (should (equal "pw" org-glance-material--password))
        (should-not (buffer-modified-p))
        (should-not (string-match-p "private body" (org-glance-graph:get-content graph "a")))))))


(ert-deftest org-glance-test:virtual-open-retains-org-hook-edits ()
  "Org hooks see source text and graph context, and their edits remain unsaved."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A" "body"))
    (let ((org-mode-hook
           (list (lambda ()
                   (when (string-prefix-p "*org-glance:" (buffer-name))
                     (should (string-match-p "body" (buffer-string)))
                     (should (equal default-directory (org-glance-graph:directory graph)))
                     (goto-char (point-max)) (insert " from hook"))))))
      (org-glance-test:with-material (buffer graph "a")
        (should (buffer-modified-p))
        (should (string-match-p "from hook" (buffer-string)))
        (should-not (string-match-p "from hook" (org-glance-graph:get-content graph "a")))
        (org-glance-material:save)
        (should-not (buffer-modified-p))
        (should (string-match-p "from hook" (org-glance-graph:get-content graph "a")))))))


(ert-deftest org-glance-test:virtual-preparation-refuses-content-identity-mismatch ()
  "Preparation validates content and metadata identity before creating an event."
  (org-glance-test:with-graph graph
    (let* ((original (org-glance-test:headline "a" "* A"))
           (other (org-glance-test:headline "b" "* B")))
      (org-glance-graph:add graph original)
      (should-error
       (org-glance-graph:prepare-edit
        graph :id "a" :contents (org-glance-headline:contents other)
        :metadata (org-glance-headline:metadata original)))
      (should (= 1 (org-glance-graph:seq graph)))
      (should (= 1 (length (org-glance-version:inventory
                           (org-glance-graph:headline-data-path graph "a"))))))))

(ert-deftest org-glance-test:virtual-pending-save-survives-clean-undo ()
  "Recover undone publications through native save, save-all and close."
  (dolist (entry '(direct all close discard))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Original" "body"))
      (org-glance-test:with-material (buffer graph "a")
        (goto-char (point-max))
        (undo-boundary)
        (insert " first edit")
        (undo-boundary)
        (let ((org-glance-version--publication-hook
               (lambda (phase)
                 (when (eq phase 'after-final-rename) (error "Injected failure")))))
          (should-error (org-glance-material:save)))
        (undo-only 1)
        (should-not (buffer-modified-p))
        (should org-glance-material--pending)
        (let ((pending org-glance-material--pending)
              (undo buffer-undo-list))
          (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?c))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (should-not (kill-buffer buffer))
            (should-not (org-glance-material--exit-query)))
          (should-not (buffer-modified-p))
          (should (eq undo buffer-undo-list))
          (org-glance-material:set-todo-bulk
           graph '("a") "DONE"
           (lambda (changed skipped)
             (should-not changed)
             (should (equal "a" (caar skipped)))))
          (should (eq pending org-glance-material--pending))
          (pcase entry
            ('direct (save-buffer))
            ('all
             (let ((save-some-buffers-default-predicate nil))
               (save-some-buffers t (lambda () nil))
               (should org-glance-material--pending)
               (set-buffer-modified-p nil)
               (save-some-buffers t)))
            ('close
             (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s)))
               (should (kill-buffer buffer))))
            ('discard
             (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?d)))
               (should (kill-buffer buffer)))))
          (if (eq entry 'discard)
              (should (= 2 (length (org-glance-version:inventory
                                   (org-glance-graph:headline-data-path graph "a")))))
            (when (buffer-live-p buffer)
              (should-not org-glance-material--pending)
              (should-not (buffer-modified-p)))
            (should-not (string-match-p "first edit" (org-glance-graph:get-content graph "a")))
            (should (= 3 (length (org-glance-version:inventory
                                 (org-glance-graph:headline-data-path graph "a")))))))))))

(ert-deftest org-glance-test:virtual-same-version-revert-refreshes-concurrency ()
  "Refresh a resolved frontier while retaining text, undo and editing state."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Original"))
    (org-glance-test:with-material (buffer graph "a")
      (let* ((parents (plist-get org-glance-material--base :frontier))
             (dir (org-glance-graph:headline-data-path graph "a"))
             racing)
        (let ((org-glance-version--publication-hook
               (lambda (phase)
                 (when (eq phase 'before-temp-write)
                   (let ((org-glance-version--publication-hook nil))
                     (setq racing (org-glance-version:write-snapshot
                                   dir "a" parents "peer"
                                   (org-glance-test:org-with-id "* Racing" "a"))))))))
          (goto-char (point-max)) (insert " local")
          (org-glance-material:save))
        (should (plist-get org-glance-material--base :reconcile))
        (let ((target (org-glance-version:path dir "reject-racer")))
          (f-mkdir-full-path target)
          (f-write-text
           (concat (json-serialize
                    (list :version 2 :event "leaves-rejected" :headline "a" :id "reject-racer"
                          :targets (vector (org-glance-version:id racing))
                          :observedLeaves (vconcat (plist-get org-glance-material--base :frontier))
                          :contentSha256 nil :created "2026-11-04T00:00:00Z" :producer "glance"))
                   "\n")
           'utf-8 (f-join target "meta.json")))
        (org-glance-cache:reconcile-inventory graph)
        (let ((text (buffer-string)) (undo buffer-undo-list)
              (position (point)) (revision org-glance-material--revision)
              (state (org-glance-cache:material-state graph "a")))
          (should (equal "live" (plist-get state :state)))
          (cl-letf (((symbol-function 'org-glance-material--load-source)
                     (lambda (&rest _) (ert-fail "Same-version revert reloaded content"))))
            (org-glance-material:revert))
          (should (equal text (buffer-string)))
          (should (eq undo buffer-undo-list))
          (should (= position (point)))
          (should (= revision org-glance-material--revision))
          (should-not (plist-get org-glance-material--base :reconcile))
          (should (equal (plist-get state :frontier)
                         (plist-get org-glance-material--base :frontier))))
        (goto-char (point-max)) (insert " next")
        (should (org-glance-publication-p (org-glance-material:save)))))))

(provide 'test-material-virtual)
