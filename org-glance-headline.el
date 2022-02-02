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
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'dash)

(defun org-glance-headline-create ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with some meta properties and `org-element' of type `headline' in contents."
  (save-excursion
    (org-glance-ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((ast (org-element-parse-buffer))
             (headline (car (org-element-contents ast)))
             (title (with-temp-buffer
                      (insert (or (org-element-property :TITLE headline)
                                  (org-element-property :raw-value headline)
                                  ""))
                      (->> (org-element-parse-buffer)
                           (org-glance-replace-links-with-titles)
                           (org-element-interpret-data)
                           (s-trim))))
             (hash (with-temp-buffer
                     (insert title)
                     (buffer-hash)))
             (tags (--map (downcase it) (org-element-property :tags headline))))

        ;; enrich basic ast with additional properties
        (org-element-put-property ast :title title)
        (org-element-put-property ast :tags (--map (intern it) tags))
        (org-element-put-property ast :id (intern (concat hash "_" (s-join "-" tags))))

        ;; no mutation restrictions on complete ast
        (org-element-put-property headline :level 1)

        ast))))

(defun org-glance-headline-id (headline)
  "Get HEADLINE ID."
  (org-element-property :id headline))

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
  (s-trim (org-element-interpret-data headline)))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
