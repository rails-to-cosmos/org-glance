;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1.0

;; Keywords: org-mode tools
;; Homepage: https://github.com/rails-to-cosmos/org-glance

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-glance-generics)
(require 'org-glance-helpers)
(require 'org-glance-index)
(require 'org-glance-scope)
(require 'org-glance-headline)
(require 'org-glance-store)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "0.1.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory t))

  ;; Read `org-glance-store' from `org-glance-directory'

  ;; Actualize `org-glance-class-store'

  ;; Headline -- node
  ;; Relationship -- (headline1, headline2, class)
  ;; Indexes on classes and timestamps
  )

(cl-defun org-glance-commit ()
  "Apply all changes of buffer headlines to its origins.

TODO:
- It should be generalized to other materialization types.
- Rebuild store indexes."
  (interactive)
  (let ((store (org-glance-store (buffer-local-value 'store (current-buffer)))))
    (apply #'org-glance-store-put store
           (-non-nil
            (org-glance-map
             (when-let (hash (org-glance-headline:get-org-property <headline> "Hash"))
               (let ((headline (org-glance-headline-remove-org-properties <headline> "Hash")))
                 (unless (string= (org-glance-hash headline) hash)
                   headline)))))))

  ;; (let ((origins (make-hash-table :test #'equal))
  ;;       (diffs (list)))



  ;;   (cl-loop for origin being the hash-keys of origins
  ;;      using (hash-values material-headlines)
  ;;      do (with-temp-file origin ;; overwrite origin on success
  ;;           (cl-destructuring-bind (header &rest origin-headlines) (org-glance-file-contents origin)
  ;;             (let ((header (s-trim header)))
  ;;               (unless (string-empty-p header)
  ;;                 (insert header "\n")))

  ;;             (cl-loop for origin-headline in origin-headlines
  ;;                do (let* ((hash (org-glance-hash origin-headline))
  ;;                          (material-headline (gethash hash material-headlines))
  ;;                          (result-headline (cond (material-headline material-headline)
  ;;                                                 (t
  ;;                                                  ;; we don't have this headline in materialization
  ;;                                                  ;; leave it as is
  ;;                                                  origin-headline))))
  ;;                     (org-glance-headline-insert result-headline))))))

  ;;   (cl-loop for diff in diffs
  ;;      do (goto-char (a-get diff :pos))
  ;;        (let ((headline (org-glance-headline-at-point)))
  ;;          (cond ((string= (a-get diff :hash) (org-glance-headline:get-org-property headline "Hash"))
  ;;                 (org-set-property "Hash" (a-get diff :modhash))
  ;;                 (message "Changes applied to headline \"%s\" " (org-glance-title headline)))
  ;;                ;; if hash function completely changes there should be problems
  ;;                (t (message "Unable to find material headline at position %d" (a-get diff :pos)))))))
  )

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode (add-hook 'before-save-hook #'org-glance-commit nil t))
        (t (remove-hook 'before-save-hook #'org-glance-commit t))))

(provide 'org-glance)
;;; org-glance.el ends here
