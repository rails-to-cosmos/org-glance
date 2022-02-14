;;; org-glance-headline.el --- headline model for `org-glance'.

;; Copyright (C) 2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 February, 2022
;; Version: 0.1.0

;; Keywords: org-glance logging
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

;;; Code:

(require 'dash)
(require 'bindat)
(require 'org-glance-helpers)

(defvar org-glance-headline-feature-bindat-spec
  '((archived byte)
    (commented byte)
    (closed byte)
    (encrypted byte)
    (linked byte)
    (propertized byte)))

(defun org-glance-headline-create ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with some meta properties and `org-element' of type `headline' in contents."
  (save-excursion
    (org-glance-ensure-at-heading)

    (save-restriction
      (org-narrow-to-subtree)
      (let* ((ast (org-element-parse-buffer))
             (subtree (org-element-contents ast))
             (contents (org-element-interpret-data subtree))
             (headline (car subtree))
             (title (with-temp-buffer
                      (insert (or (org-element-property :TITLE headline)
                                  (org-element-property :raw-value headline)
                                  ""))
                      (->> (org-element-parse-buffer)
                           org-glance-replace-links-with-titles
                           org-element-interpret-data
                           substring-no-properties
                           s-trim)))
             (tags (--map (downcase it) (org-element-property :tags headline))))

        ;; enrich ast with additional properties
        (org-element-put-property ast :title title)
        (org-element-put-property ast :tags (--map (intern it) tags))
        (org-element-put-property ast :hash (with-temp-buffer
                                              (insert title)
                                              (buffer-hash)))

        (org-element-put-property ast :location (list :file (buffer-file-name)
                                                      :buffer (current-buffer)))

        (org-element-put-property ast :features (cl-flet ((bool-to-int (bool) (if (null bool) 0 1)))
                                                  (bindat-pack
                                                   org-glance-headline-feature-bindat-spec
                                                   (list (cons 'archived (bool-to-int (org-element-property :archivedp headline)))
                                                         (cons 'commented (bool-to-int (org-element-property :commentedp headline)))
                                                         (cons 'closed (bool-to-int (org-element-property :closed headline)))
                                                         (cons 'encrypted (bool-to-int (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
                                                         (cons 'linked (bool-to-int (s-match-strings-all org-link-any-re contents)))
                                                         (cons 'propertized (bool-to-int (s-match-strings-all "^\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)$" contents)))))))

        ;; normalize indentation
        (let ((indent-offset (1- (org-element-property :level headline))))
          (when (> indent-offset 0)
            (cl-loop
               for headline in (org-element-map subtree 'headline #'identity)
               for level = (org-element-property :level headline)
               do (org-element-put-property headline :level (- level indent-offset)))))

        ast))))

;; (defun org-glance-headline-id (headline)
;;   "Get HEADLINE ID."
;;   (org-element-property :id headline))

(defun org-glance-headline-title (headline)
  "Get HEADLINE title."
  (org-element-property :title headline))

(defun org-glance-headline-hash (headline)
  "Get HEADLINE hash."
  (org-element-property :hash headline))

(defun org-glance-headline-tags (headline)
  "Get HEADLINE tags."
  (org-element-property :tags headline))

(defun org-glance-headline-contents (headline)
  "Interpret HEADLINE contents."
  (->> headline
       org-element-contents
       org-element-interpret-data
       substring-no-properties
       s-trim))

(defun org-glance-headline-dump (headline)
  (cadr headline))

(defun org-glance-headline-save (headline file)
  "Write HEADLINE to FILE."
  (with-temp-file file
    (insert (org-glance-headline-contents headline))))

(defun org-glance-headline-load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (org-glance-headline-create)))

(defun org-glance-headline-features (headline)
  (bindat-unpack org-glance-headline-feature-bindat-spec
                 (org-element-property :features headline)))

(defun org-glance-headline-archived-p (headline)
  (= 1 (alist-get 'archived (org-glance-headline-features headline))))

(defun org-glance-headline-commented-p (headline)
  (= 1 (alist-get 'commented (org-glance-headline-features headline))))

(defun org-glance-headline-closed-p (headline)
  (= 1 (alist-get 'closed (org-glance-headline-features headline))))

(defun org-glance-headline-encrypted-p (headline)
  (= 1 (alist-get 'encrypted (org-glance-headline-features headline))))

(defun org-glance-headline-linked-p (headline)
  (= 1 (alist-get 'linked (org-glance-headline-features headline))))

(defun org-glance-headline-propertized-p (headline)
  (= 1 (alist-get 'propertized (org-glance-headline-features headline))))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
