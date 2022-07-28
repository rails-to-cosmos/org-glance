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

(cl-defun org-glance-commit (&optional (store (org-glance-store (buffer-local-value 'store (current-buffer)))))
  "Apply all changes of buffer headlines to its origins in STORE.

TODO:
- It should be generalized to other materialization types.
- [x] Rebuild store indexes."
  (interactive)
  (when (local-variable-p 'org-glance-material-marks)
    (cl-maphash (lambda (key val)
                  (let* ((beg (a-get val :beg))
                         (end (a-get val :end))
                         (hash (a-get val :hash))
                         (headline (save-excursion
                                     (goto-char beg)
                                     (org-glance-headline-at-point))))
                    (if (not (string= hash (org-glance-hash headline)))
                        (message "Headline \"%s\" has been removed from materialization. Changes ignored."
                                 (org-glance-headline-title headline))
                      (org-glance-store-put store headline)
                      (let ((overlay (get-text-property beg :overlay)))
                        (when overlay (delete-overlay overlay))
                        (let ((overlay (make-overlay beg (1+ beg))))
                          (add-text-properties beg end (list :hash (org-glance-headline-hash headline)
                                                             :changed-p nil
                                                             :overlay overlay))
                          ;; FIXME when C-k C-k it draws next headline instead of origin
                          (overlay-put overlay 'face '(:foreground "#27ae60")))))))
                org-glance-material-marks)
    (setq-local org-glance-material-marks (make-hash-table :test #'equal)))
  ;; TODO remove old headline from store or mark for deletion
  ;; TODO work with stale links (broken)
  )

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar-local org-glance-material-marks (make-hash-table :test #'equal))

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (let ((store (org-glance-store (buffer-local-value 'store (current-buffer)))))
           (org-glance-map (headline)
             (let* ((hash (org-glance-hash headline))
                    (synced-p (when (org-glance-store-get store hash)
                                t)))
               (add-text-properties (point-min) (point-max)
                                    (list :hash (if synced-p
                                                    hash
                                                  nil)
                                          :overlay nil
                                          :changed-p nil
                                          :synced-p synced-p))
               (unless synced-p
                 (let ((x (make-overlay (point-min) (1+ (point-min)))))
                   (overlay-put x 'face '(:foreground "#e74c3c")))))))
         (add-hook 'post-command-hook #'org-glance-material-debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-mark nil t)
         (add-hook 'before-save-hook #'org-glance-commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mark t)
         (remove-hook 'post-command-hook #'org-glance-material-debug t))))

(cl-defun org-glance-material-debug (&rest _)
  (message "%d %s %s %s %s"
           (point)
           org-glance-material-marks
           (get-text-property (point) :changed-p)
           (get-text-property (point) :synced-p)
           (get-text-property (point) :hash)))

(cl-defun org-glance-material-mark (&rest _)
  "Mark current headline as changed in current buffer."
  (while-no-input
    (redisplay)
    (unless (local-variable-p 'org-glance-material-marks)
      (setq-local org-glance-material-marks (make-hash-table :test #'equal)))

    (org-glance--with-heading-at-point
      (let* ((headline (org-glance-headline-at-point)) ;; FIXME do not construct headline in future, optimize me
             (overlay (get-text-property (point) :overlay))
             (changed-p (get-text-property (point) :changed-p))
             (hash-old (get-text-property (point) :hash))
             (hash-new (org-glance-hash headline))
             (returned-to-unchanged-state-p (and (string= hash-old hash-new) changed-p))
             (first-change-p (and (not (string= hash-old hash-new)) (not changed-p)))
             (further-change-p (and (not (string= hash-old hash-new)) changed-p))
             (unsynced-p (not (get-text-property (point) :synced-p))))
        (cond
          (unsynced-p
           ;; skip it or prompt user to add it to store
           (when (yes-or-no-p "Attempt to change unsynced headline. Do you want to add it to store?")
             ;; TODO sync it
             ))
          (returned-to-unchanged-state-p
           (when overlay (delete-overlay overlay))
           (remhash hash-old org-glance-material-marks)
           (add-text-properties (point-min) (point-max)
                                (list :changed-p nil
                                      :overlay nil)))
          (first-change-p
           (when overlay (delete-overlay overlay))
           (let ((overlay (make-overlay (point-min) (1+ (point-min)))))
             (overlay-put overlay 'face '(:foreground "#ffcc00"))
             (puthash hash-old (a-list :beg (point-min)
                                       :end (point-max)
                                       :hash hash-new
                                       :overlay overlay)
                      org-glance-material-marks)
             (add-text-properties (point-min) (point-max)
                                  (list :changed-p t
                                        :overlay overlay))))
          (further-change-p
           (puthash hash-old (a-list :beg (point-min)
                                     :end (point-max)
                                     :hash hash-new
                                     :overlay overlay)
                    org-glance-material-marks)))))))

(provide 'org-glance)
;;; org-glance.el ends here
