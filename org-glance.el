;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1

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

(require 's)
(require 'org)

(defun org-glance (&rest args)
"Use optional ARGS to customize your glancing blows:
- SCOPE :: org-file or SCOPE from org-map-entries (org.el)
- PROMPT :: completing read title (default: \"Glance: \")
- SEPARATOR :: completing read entry separator (default: \" → \")
- FILTER :: lambda to filter entries in completing read or
  - 'links-only :: remove all non-link entries
- ACTION :: lambda to call on selected entry
  - if entry has an org-link in title, browse it
  - if entry has HANDLER property, read-eval it
- HANDLER :: property name to read-eval on select (default: \"HANDLER\")

\(fn [:scope SCOPE] [:prompt PROMPT] [:separator SEPARATOR] [:filter FILTER] [:action ACTION] [:handler HANDLER])"
  (let* ((scope     (or (plist-get args :scope)          nil))

         (filter    (or (plist-get args :filter)         (lambda nil t)))
         (filter*   (cond ((eq filter 'links-only) (lambda () (org-match-line (format "^.*%s.*$" org-bracket-link-regexp))))
                          (t filter)))

         (handler   (or (plist-get args :handler)        "HANDLER"))
         (prompt    (or (plist-get args :prompt)         "Glance: "))
         (separator (or (plist-get args :separator)      " → "))
         (action    (or (plist-get args :action)         (lambda nil (org-glance/handle-entry handler)))))
    (cl-flet ((traverse ()
                        (let* ((mark (point-marker))
                               (title (s-join separator (org-get-outline-path t))))
                          (when (funcall filter*)
                            (cons title mark)))))

      (let* ((entry-map (if (and (stringp scope) (file-exists-p scope))
                            (with-temp-buffer
                              (org-mode)
                              (insert-file-contents-literally scope)
                              (org-map-entries #'traverse))
                          (org-map-entries #'traverse scope)))
             (clean-entries (remove 'nil entry-map))
             (marker (cdr (assoc-string (org-completing-read prompt entry-map) entry-map))))
        (org-goto-marker-or-bmk marker)
        (funcall action)))))

(defun org-glance/handle-entry (handler)
"Try to handle current org-entry:
1. If there is an org-link, browse it.
2. If not, call HANDLER."
  (cond ((org-match-line (format "^.*%s.*$" org-bracket-link-regexp)) (org-glance/follow-org-link-at-point))
        ((org-entry-get nil handler) (eval (read (org-entry-get nil handler))))))

(defun org-glance/follow-org-link-at-point ()
"Browse org-link at point."
  (let ((link (buffer-substring-no-properties
               (save-excursion (org-beginning-of-line) (point))
               (save-excursion (org-end-of-line) (point))))
        (org-link-frame-setup (acons 'file 'find-file org-link-frame-setup)))
    (org-open-link-from-string link)))

(provide 'org-glance)
;;; org-glance.el ends here
