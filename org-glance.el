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
(require 'org-glance-headline)

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

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines on complete."
  :group 'org-glance
  :type 'boolean)

(defvar org-glance-material-buffers (make-hash-table))

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory t))

  ;; Read `org-glance-registry' from `org-glance-directory'

  ;; Actualize `org-glance-class-registry'

  ;; Storage partitioning schema: class/created-date/headline-id/headline.el
  ;; Archive partitioning schema: class/closed-date/headline-id/headline.el
  )

(cl-defmacro org-glance-loop (&rest forms)
  "Loop over headlines and execute FORMS on each.
This is the anaphoric method, you can use `_' to call headline in forms."
  `(cl-loop for pos in (-non-nil
                        (org-element-map (org-element-parse-buffer 'headline) 'headline
                          (lambda (headline)
                            (when (= (org-element-property :level headline) 1)
                              (org-element-property :begin headline)))))
      collect (save-excursion
                (goto-char pos)
                (let ((<headline> (org-glance-headline-at-point)))
                  (org-glance:with-heading-at-point
                    ,@forms)))))

(cl-defmacro org-glance-with-file (file &rest forms)
  (declare (indent 1))
  `(with-temp-file ,file
     (org-mode)
     ,@forms))

(cl-defmacro org-glance-loop-file (file &rest forms)
  (declare (indent 1))
  `(org-glance-with-file ,file
     (insert-file-contents-literally ,file)
     (org-glance-loop
      (setf (slot-value <headline> 'file) ,file)
      ,@forms)))

(cl-defmacro org-glance-loop-file-1 (file &rest forms)
  (declare (indent 1))
  `(car (-non-nil (org-glance-loop-file ,file ,@forms))))

(cl-defmethod org-glance-materialize ((file string) (headline org-glance-headline))
  (declare (indent 1))
  (org-glance-with-file file
    (if-let (file (org-glance-headline:file headline))
        (progn
          (org-glance-headline:set-property headline "Hash" (org-glance-headline:hash headline))
          (org-glance-headline:set-property headline "Origin" (org-glance-headline:file headline))
          (org-glance-headline:insert headline))
      (warn "Unable to materialize headline without file origin"))))

(cl-defmethod org-glance-materialize ((file string) (headlines list))
  (declare (indent 1))
  (org-glance-with-file file
    (--map (if-let (file (org-glance-headline:file it))
               (progn
                 (org-glance-headline:set-property it "Hash" (org-glance-headline:hash it))
                 (org-glance-headline:set-property it "Origin" (org-glance-headline:file it))
                 (org-glance-headline:insert it))
             (warn "Unable to materialize headline without file origin"))
           headlines)))

(cl-defun org-glance-commit ()
  "Apply all changes of buffer headlines to its origins."
  (interactive)

  (let ((origins (make-hash-table :test #'equal))
        (diffs (list)))

    (org-glance-loop
     (when-let (hash (org-glance-headline:pop-property <headline> "Hash"))
       (let ((origin (org-glance-headline:pop-property <headline> "Origin"))
             (modhash (org-glance-headline:hash <headline>)))

         (cond ((gethash origin origins)
                (puthash hash <headline> (gethash origin origins)))
               (t (let ((new (make-hash-table :test #'equal)))
                    (puthash hash <headline> new)
                    (puthash origin new origins))))

         (unless (string= modhash hash)
           (cl-pushnew (a-list :hash modhash
                               :pos (point))
                       diffs)))))

    (cl-loop for origin being the hash-keys of origins
       using (hash-values headlines)
       do (with-temp-file origin  ;; overwrite origin on success
            (-map #'org-glance-headline:insert
                  (org-glance-loop-file origin
                    (let* ((hash (org-glance-headline:hash <headline>))
                           (uncommitted (gethash hash headlines)))
                      (cond (uncommitted
                             ;; we found origin of materialized headline
                             ;; but do we need to make any changes?
                             ;; let's check hashes
                             uncommitted)
                            (t
                             ;; we don't have this headline in materialization
                             ;; leave it as is
                             <headline>)))))))

    (cl-loop for diff in diffs
       do (goto-char (a-get diff :pos))
         (org-set-property "Hash" (a-get diff :hash))))

  ;; (org-glance-loop
  ;;  (let* ((changed headline)
  ;;         (origin (org-glance-headline:pop-property changed "Origin"))
  ;;         (src-hash (org-glance-headline:pop-property changed "Hash"))
  ;;         (new-hash (when (and origin src-hash)
  ;;                     (org-glance-loop-file-1 origin
  ;;                       (when (string= src-hash (org-glance-headline:hash headline))
  ;;                         (delete-region (point-min) (point-max))
  ;;                         (insert (org-glance-headline:contents changed))
  ;;                         (org-glance-headline:hash changed))))))
  ;;    (cond ((null new-hash) (user-error "Origin not found: %s" src-hash))
  ;;          ((string= new-hash src-hash) (message "Headline is up to date"))
  ;;          (t (org-set-property "Hash" new-hash)))))
  )

(org-glance-materialize "/tmp/material.org"
                        (org-glance-loop-file "/tmp/input.org"
                          <headline>))

;; (org-glance-loop-file "/tmp/org-glance-UUPIZF/tmp/phones.org"
;;   (org-glance-headline:hash headline))


(provide 'org-glance)
;;; org-glance.el ends here
