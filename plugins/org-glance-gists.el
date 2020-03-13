;;; org-glance-gists.el --- fast src-blocks retrieval utility.

;; Copyright (C) 2019-2020 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 Dec 2019
;; Version: 0.1

;; Keywords: babel org tools
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

(require 'load-relative)

(defvar og-gists-cache-file "~/.emacs.d/org-glance/gists.el")

(defun org-glance-act--gist-execute (headline)
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file))
         (case-fold-search t))

    (if (file-exists-p file)
        (find-file file)
      (org-glance-cache-outdated "File not found: %s" file))

    (goto-char point)
    ;; at headline

    (org-show-subtree)
    (search-forward "#+BEGIN_SRC")
    (beginning-of-line)
    ;; at src block

    (assert (org-in-src-block-p))
    ;; assert block in proper gist
    (let ((block-headers (nth 2 (org-babel-get-src-block-info))))
      (loop for pair in block-headers
            when (eq (car pair) :var)
            collect
            (let* ((key (cadr pair))
                   (prompt (format "%s: " (symbol-name key)))
                   (pvals (cddr pair))
                   (compls (s-split "|" pvals)))
              (if (> (length compls) 1)
                  (list key (org-completing-read prompt compls))
                (list key (car compls))))))))

(defun org-glance-gists-execute (&optional force-reread-p)
  (interactive "P")
  (org-glance
   :scope '(agenda-with-archives)
   :prompt "Execute gist: "
   :cache-file og-gists-cache-file
   :fallback (lambda (x) (user-error "Gist not found."))
   :title-property :TITLE
   :filter (lambda (headline) (-contains? (org-element-property :tags headline) "Gist"))
   :force-reread-p force-reread-p
   :action #'org-glance-act--gist-execute))

(defun org-glance-gists-visit (&optional force-reread-p)
  (interactive "P")
  (org-glance
   :scope '(agenda-with-archives)
   :prompt "Visit gist: "
   :cache-file og-gists-cache-file
   :fallback (lambda (x) (user-error "Gist not found."))
   :title-property :TITLE
   :filter (lambda (headline) (-contains? (org-element-property :tags headline) "Gist"))
   :force-reread-p force-reread-p
   :action #'org-glance-act--visit-headline))

(provide-me)
;;; org-glance-gists.el ends here
