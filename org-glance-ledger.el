(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'org)
  (require 'ts)
  (require 'ledger-mode)
  (require 'load-relative))

(defconst org-glance-ledger-commodity-regexp
  "\\(\"[^\"]+\"\\|[^]!&(-/:-@[^{-}[:digit:][:blank:]]+\\)")

(defconst org-glance-ledger-amount-regex
  (concat
   "\\(?:" org-glance-ledger-commodity-regexp " *\\)?"
   ;; We either match just a number after the commodity with no
   ;; decimal or thousand separators or a number with thousand
   ;; separators.  If we have a decimal part starting with `,'
   ;; or `.', because the match is non-greedy, it must leave at
   ;; least one of those symbols for the following capture
   ;; group, which then finishes the decimal part.
   "\\(-?\\(?:[0-9]+\\|[0-9,.]+?\\)\\)"
   "\\([,. ][0-9)]+\\)?"
   "\\(?: *" org-glance-ledger-commodity-regexp "\\)?"
   "\\([ \t]*[@={]@?[^\n;]+?\\)?"
   "\\([ \t]+;.+?\\|[ \t]*\\)?$"))

(defun re-seq (regexp string)
  "Get a list of all REGEXP matches in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (cl-loop while (string-match regexp string pos)
               do (progn
                    (push (match-string 0 string) matches)
                    (setq pos (match-end 0))))
      matches)))

(defun org-glance-ledger--build-report-from-subtree-at-point ()
  (interactive)

  (org-back-to-heading)

  (unless (org-at-heading-p)
    (user-error "Not at heading"))

  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((tag-string (org-make-tag-string (org-get-tags)))
             (tags (if (string-empty-p tag-string) ":" tag-string))
             (title (org-entry-get (point) "ITEM"))
             (contents (buffer-substring-no-properties
                        (point-min)
                        (save-excursion
                          (if (org-goto-first-child)
                              (point)
                            (point-max)))))
             (ts (-some->>
                     (re-seq org-ts-regexp contents)
                   (-sort #'s-less?)
                   (car)
                   (ts-parse-org)))
             (amounts (re-seq org-glance-ledger-amount-regex contents))
             (output-filename (make-temp-file "org-glance-ledger-" nil ".org")))
        (unless ts
          (user-error "Timestamp not found in subtree contents."))
        (with-current-buffer-window
         "*org-glance-ledger-report*" nil nil
         (ledger-mode)
         (insert (format "%04d/%02d/%02d %s\n" (ts-year ts) (ts-month ts) (ts-day ts) title))
         (loop for amount in amounts
               do (insert (format "    Expenses%s%s    %s\n" tags title amount)))
         (insert "    Assets:Default\n")
         (switch-to-buffer-other-window "*org-glance-ledger-report*"))))))

(provide-me)
