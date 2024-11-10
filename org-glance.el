;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode mindmap.

;; Copyright (C) 2018-2024 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (aes) (dash) (f) (highlight) (transient) (elsa) (with-simulated-input))
;; Keywords: org-mode, graph, mindmap
;; Homepage: https://github.com/rails-to-cosmos/org-glance
;; Source: gnu, melpa, org
;; License: GPL-3+

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

(require 'aes)
(require 'cl-generic)
(require 'cl-lib)
(require 'cl-macs)
(require 'dash)
(require 'f)
(require 'json)
(require 'ol)
(require 'org)
(require 'org-element)
(require 's)
(require 'seq)
(require 'subr-x)

(require 'org-glance-customs)
(require 'org-glance-utils)

(require 'org-glance-tag)
(require 'org-glance-headline)
(require 'org-glance-exceptions)
(require 'org-glance-overview-mode)
(require 'org-glance-material-mode)
(require 'org-glance-datetime-mode)
(require 'org-glance-ui)


(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-tags (make-hash-table)
  "Hash table {tag id -> tag parameters}")

(cl-defun org-glance:tags ()  ;; -> list[symbol]
  (hash-table-keys org-glance-tags))

(cl-defun org-glance:tags-sorted () ;; -> list[symbol]
  (sort (org-glance:tags) #'s-less?))

(cl-defun org-glance:tag-metadata-file-name (tag)  ;; -> string
  (format "%s/%s/%s.metadata.el" org-glance-directory tag tag))

;; TODO refactor is needed for all the filters
(cl-defun org-glance:tag-filter (tag) ;; -> callable
  #'(lambda (headline)
      (when (-contains? (mapcar #'downcase (org-element-property :tags headline)) (symbol-name tag))
        headline)))

(cl-defun org-glance:tag-headlines (tag) ;; -> list[headline]
  (org-glance-headlines :db (org-glance:tag-metadata-file-name tag)
                        :scope (list org-glance-directory)
                        :filter (org-glance:tag-filter tag)))

(cl-defun org-glance-tags:completing-read (&optional (prompt "Tag: ") (require-match t))
  "Run completing read PROMPT on registered tags filtered by TYPE."
  (let ((tags (org-glance:tags-sorted)))
    (if (= (length tags) 1)
        (car tags)
      (intern (completing-read prompt tags nil require-match)))))

(cl-defun org-glance:capture-template (tag &key (default ""))
  (let ((capture-template-config-file (f-join (org-glance-overview:directory tag) "capture-template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

(defun org-glance:encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance:decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(cl-defun org-glance:tag-file-name (&optional (tag (org-glance-tags:completing-read)))
  "Path to directory where TAG-ID resources and metadata are stored."
  (abbreviate-file-name (f-join org-glance-directory (s-downcase (format "%s" tag)) "resources")))

(cl-defun org-glance:tag-metadata (tag)
  (->> tag
       org-glance:tag-metadata-file-name
       org-glance-metadata:read))

(cl-defun org-glance:make-tag-directory (&optional (tag (org-glance-tags:completing-read)))
  (save-excursion
    (org-glance--back-to-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (org-glance-headline:make-directory
       (org-glance:tag-file-name tag)
       (org-element-property :raw-value (org-element-at-point))))))

(cl-defun org-glance:create-tag (tag)
  (unless (and (symbolp tag) (symbol-downcased-p tag))
    (error "Tag should be a downcased symbol."))

  (when (org-glance-tag:register tag org-glance-tags)
    (org-glance-metadata:create (org-glance:tag-metadata-file-name tag))
    (org-glance-overview:create tag)))

(cl-defun org-glance-materialized-headline:preserve-history-before-auto-repeat (&rest _)
  (when (and org-glance-clone-on-repeat-p
             (or org-glance-material-mode org-glance-overview-mode)
             (member (org-get-todo-state) org-done-keywords)
             (org-glance-headline:repeated-p))
    (let ((contents (org-glance-headline-contents)))
      (run-with-idle-timer 1 nil
                           #'(lambda () (save-window-excursion
                                     (with-temp-buffer
                                       (insert contents)
                                       (goto-char (point-min))

                                       (org-glance-datetime-reset-buffer-timestamps-except-earliest)

                                       (cl-loop
                                        for class in (org-glance-headline:tags (org-glance-headline:at-point))
                                        do (let ((headline (org-glance-capture-headline-at-point class)))
                                             (org-glance-overview:register-headline-in-archive headline class))))))))))

(cl-defun org-glance-materialized-headline:cleanup-after-auto-repeat (&rest _)
  "Do only if headline has been cloned before auto repeat.
Cleanup new headline considering auto-repeat ARGS.

- Remove all data but PINNED of cloned headline."
  (when (and (or org-glance-material-mode org-glance-overview-mode)
             org-glance-clone-on-repeat-p
             (org-glance-headline:repeated-p))
    (let ((contents (org-glance-headline:with-headline-at-point
                     (let ((header (s-trim (buffer-substring-no-properties (point) (save-excursion (org-end-of-meta-data) (point)))))
                           (pinned (save-excursion
                                     (cl-loop
                                      while (search-forward "#+begin_pin" nil t)
                                      collect (save-excursion
                                                (beginning-of-line)
                                                (buffer-substring-no-properties (point) (save-excursion
                                                                                          (search-forward "#+end_pin" nil t)
                                                                                          (point))))))))
                       (s-join "\n\n" (append (list header) pinned))))))
      (delete-region (point-min) (point-max))
      (insert contents)
      (org-delete-property "LAST_REPEAT"))))

;; TODO refactor
(cl-defmacro org-glance-choose-and-apply (&key filter action)
  "If HEADLINE specified, apply ACTION on it.

If HEADLINE is not specified, ask user to choose HEADLINE from
existing headlines filtered by FILTER.

If user chooses unexisting headline, capture it and apply ACTION
after capture process has been finished."
  `(condition-case default
       (cond (,filter (funcall ,action (org-glance-metadata:choose-headline :filter ,filter)))
             (t (funcall ,action (org-glance-metadata:choose-headline))))
     (HEADLINE-NOT-FOUND
      (let ((<buffer> (current-buffer))
            (<point> (point)))
        (org-glance-capture
         :default (cadr default)
         :class (org-glance-tags:completing-read "Unknown headline. Please, specify it's tag to capture: ")
         :callback (lambda ()
                     (let ((<hl> (org-glance-overview:original-headline)))
                       (switch-to-buffer <buffer>)
                       (goto-char <point>)
                       (funcall ,action <hl>))))))))

(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Update all changed entities from `org-glance-directory'."

  (unless (f-exists? directory)
    (mkdir directory t))

  (org-glance-overview-init)

  (add-hook 'org-glance-material-mode-hook #'org-glance-datetime-mode)
  (advice-add 'org-auto-repeat-maybe :before #'org-glance-materialized-headline:preserve-history-before-auto-repeat (list :depth -90))
  (advice-add 'org-auto-repeat-maybe :after #'org-glance-materialized-headline:cleanup-after-auto-repeat)
  (advice-add 'org-glance-headline:materialize :around #'org-glance-enable-encrypted-headlines)

  (cl-loop for dir in (org-glance--list-directories directory)
           for tag = (org-glance-tag:read dir)
           do (org-glance:create-tag tag))

  (cl-loop for tag being the hash-keys of org-glance-tags
           unless (f-exists? (f-join directory (org-glance-tag:file-name tag)))
           do (org-glance-tag:remove tag org-glance-tags))

  (setq org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))

(cl-defun org-glance:@ ()
  "Choose headline to refer. Insert link to it at point."
  (interactive)
  (let ((active-region? (and (not (org-in-src-block-p)) (region-active-p)))
        (mention? (and (not (org-in-src-block-p)) (or (looking-back "^" 1) (looking-back "[[:space:]]" 1)))))
    (condition-case nil
        (cond (active-region? (let ((<buffer> (current-buffer))
                                    (<region-beginning> (region-beginning))
                                    (<region-end> (region-end)))
                                (org-glance-capture :default (buffer-substring-no-properties <region-beginning> <region-end>)
                                                    :class (org-glance-tags:completing-read (format "Specify class for \"%s\": " (buffer-substring-no-properties <region-beginning> <region-end>)))
                                                    :finalize t
                                                    :callback (lambda () (let ((headline (org-glance-overview:original-headline)))
                                                                      (switch-to-buffer <buffer>)
                                                                      (goto-char <region-beginning>)
                                                                      (delete-region <region-beginning> <region-end>)
                                                                      (insert (org-glance-headline:with-narrowed-headline headline
                                                                                (org-glance-headline-reference))))))))

              (mention? (org-glance-choose-and-apply
                         :action (lambda (headline)
                                   (insert
                                    (org-glance-headline:with-narrowed-headline headline
                                      (org-glance-headline-reference))))))

              ;; simple @
              (t (keyboard-quit)))
      (quit (self-insert-command 1 64)))))

(cl-defun org-glance:materialize (&optional headline)
  "Materialize HEADLINE in a new buffer."
  (interactive)
  (let ((action (lambda (headline)
                  (let ((buffer (org-glance-materialized-headline-buffer headline)))
                    (switch-to-buffer
                     (if (buffer-live-p buffer)
                         buffer
                       (org-glance-headline:materialize headline)))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
       :filter #'org-glance-headline:active?
       :action action))))

(cl-defun org-glance:open (&optional headline)
  "Run `org-open-at-point' on any `org-link' inside HEADLINE.
If there is only one link, open it.
If there is more than one link, prompt user to choose which one to open.
If headline doesn't contain links, role `can-be-opened' should be revoked."
  (interactive)
  (let ((action (lambda (headline)
                  (org-glance:with-headline-materialized headline
                    (cl-loop for (link title pos) in (org-glance--parse-links)
                             unless (s-starts-with-p "[[org-glance" link)
                             collect (list title pos)
                             into links
                             finally
                             do (goto-char (cond ((> (length links) 1) (cadr (assoc (completing-read "Open link: " links nil t) links #'string=)))
                                                 ((= (length links) 1) (cadar links))
                                                 (t (user-error "Unable to find links in headline"))))
                             (org-open-at-point))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
       :filter (lambda (headline)
                 (and
                  (org-glance-headline:active? headline)
                  (org-glance-headline:linked? headline)))
       :action action))))

(cl-defun org-glance:extract (&optional headline)
  "Materialize HEADLINE and retrieve key-value pairs from its contents."
  (interactive)
  (let ((action (lambda (headline)
                  (let ((pairs (org-glance:with-headline-materialized headline
                                 (org-glance--buffer-key-value-pairs))))
                    (condition-case nil
                        (while t
                          (kill-new (alist-get (completing-read "Extract property: " pairs nil t) pairs nil nil #'string=)))
                      (quit
                       (setq kill-ring nil)
                       (message "Kill ring has been cleared")))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
       :filter (lambda (headline)
                 (pp headline)
                 (and
                  (org-glance-headline:active? headline)
                  (or (org-glance-headline:propertized? headline)
                      (org-glance-headline:encrypted? headline))))
       :action action))))

;; (cl-defun org-glance:prototype ()
;;   (interactive)
;;   "Capture headline based on chosen prototype."
;;   (org-glance-choose-and-apply
;;    :action (lambda (headline)
;;              (org-glance-capture
;;               :class (org-element-property :class headline)
;;               :template (org-glance-headline-contents headline)))))

(cl-defun org-glance-capture (&key (tag (org-glance-tags:completing-read))
                                   (file (make-temp-file "org-glance-" nil ".org"))
                                   (default (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                                                  (t "")))
                                   (callback nil)
                                   (finalize nil)
                                   (template (org-glance:capture-template tag :default default)))
  (interactive)
  (let ((tag (if (symbolp tag) tag (intern tag))))
    (find-file file)
    (setq-local org-glance-capture:id (org-glance-tag:id* tag)
                org-glance-capture:tag tag
                org-glance-capture:default default)
    (add-hook 'org-capture-prepare-finalize-hook 'org-glance-capture:prepare-finalize-hook 0 t)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-capture:after-finalize-hook 0 t)
    (when callback (add-hook 'org-capture-after-finalize-hook callback 1 t))
    (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
      (org-capture nil "_")
      (when finalize (org-capture-finalize)))))

;; (cl-defun org-glance-headline-remove ()
;;   (interactive)
;;   (org-glance-choose-and-apply
;;    :action (lambda (headline)
;;              (org-glance:with-headline-materialized headline
;;                (org-set-tags '())))))

(cl-defun org-glance:insert-pin-block ()
  (interactive)
  (insert "#+begin_pin" "\n\n" "#+end_pin")
  (forward-line -1))

;; Headline

(cl-defun org-glance-headline:search-backward ()
  (interactive)
  (when-let (headline (org-glance-headline:at-point))
    (goto-char (org-glance-headline:begin headline))
    (forward-char -1))

  (while (and (not (org-before-first-heading-p))
              (not (bobp))
              (not (org-glance-headline:at-point)))
    (outline-previous-heading))

  (if-let (headline (org-glance-headline:at-point))
      (progn (goto-char (org-glance-headline:begin headline))
             headline)
    (progn (goto-char (point-min)))))

(cl-defun org-glance-headline:search-forward ()
  (interactive)

  (let ((headline (org-glance-headline:at-point))
        next-headline)

    (save-excursion

      (when headline
        (if (org-glance-headline:end headline)
            (goto-char (org-glance-headline:end headline))
          (goto-char (point-max)))

        (condition-case nil
            (progn (beginning-of-line)
                   (forward-line 1))
          (end-of-buffer nil)))

      (setq next-headline (org-glance-headline:at-point))
      (while (and (not (eobp))
                  (or (not next-headline)
                      (equal headline next-headline)))
        (forward-line 1)
        (setq next-headline (org-glance-headline:at-point))))

    (when (and next-headline (not (equal headline next-headline)))
      (goto-char (org-glance-headline:begin next-headline))
      next-headline)))

(cl-defun org-glance-headline:list ()
  (save-excursion
    (goto-char (point-min))

    (let (result)
      (when-let (headline (org-glance-headline:at-point))
        (push headline result))

      (while-let ((headline (org-glance-headline:search-forward)))
        (push headline result))

      result)))

(cl-defun org-glance:headline-title (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE, cleanup links."
  (with-temp-buffer
    (save-excursion (insert (org-glance-headline:title headline)))
    (org-glance--remove-links 'org-glance-overview 'org-glance-state)
    (org-glance--substitute-links)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance:headline-alias (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE considering alias property."
  (with-temp-buffer
    (save-excursion
      (insert (org-glance-headline:alias headline)))
    (org-glance--remove-links 'org-glance-overview 'org-glance-state)
    (org-glance--substitute-links)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (element) (when (string= (org-glance-headline:id element) id)
                                 (org-element-property :begin element))))))

    (unless points
      (HEADLINE-NOT-FOUND "Headline not found in file %s: %s" (buffer-file-name) id))

    (when (> (length points) 1)
      (message "Headline ID %s is not unique in file %s" id (buffer-file-name)))

    (goto-char (car points))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:visit (&optional (headline (org-glance-headline:at-point)))
  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file-name headline))
         (buffer (org-glance-headline:buffer headline))
         (revert-without-query (list file)))

    (cond ((and file (file-exists-p file)) (find-file file))
          ((and buffer (buffer-live-p buffer)) (switch-to-buffer buffer))
          (t (message "File and buffer not found for visiting. Using current buffer...")))

    (widen)

    (cond (id (org-glance-headline:search-buffer-by-id id))
          (t (goto-char (org-glance-headline:begin headline))))))

(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance--back-to-heading)
  (while (and (org-glance-headline? (org-glance-headline:at-point)) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:replace-headline-at-point (new-contents)
  (let ((beg (org-glance-headline:begin (org-glance-headline:at-point)))
        (end (save-excursion (org-end-of-subtree t)))
        (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert new-contents)))

(cl-defun org-glance-headline-contents (&optional (headline (org-glance-headline:at-point)))
  "Extracts HEADLINE contents.
FIXME. Unstable one. Refactor is needed."
  (let ((file (org-glance-headline:file-name headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond (file (with-temp-buffer
                  (org-mode)
                  (insert-file-contents file)
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (org-glance-headline:promote-to-the-first-level)
                  (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer (with-current-buffer buffer
                    (save-window-excursion
                      (save-excursion
                        (save-restriction
                          (widen)
                          (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                          (org-narrow-to-subtree)
                          (let ((contents (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
                            (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (goto-char (point-min))
                              (outline-next-heading)
                              (org-glance-headline:promote-to-the-first-level)
                              (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))))
          (t (HEADLINE-NOT-FOUND "Unable to determine headline location")))))

(cl-defgeneric org-glance-headline:extract-from (scope)
  "Extract `org-glance-headlines' from scope.")

(cl-defmethod org-glance-headline:extract-from ((f string))
  "Extract headlines from file F."
  (if-let (b (get-buffer f)) ;; buffer name
      (org-glance-headline:extract-from b)
    (with-temp-buffer
      (message "Scan file %s" f)
      (insert-file-contents f)
      (org-mode)
      (cl-loop
       for headline in (org-glance-headline:extract-from (current-buffer))
       collect (org-glance-headline:update headline :file (abbreviate-file-name f))))))

(cl-defmethod org-glance-headline:extract-from ((b buffer))
  "Extract headlines from buffer B."
  (with-current-buffer b
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (org-glance-headline? headline)
          (save-excursion
            (goto-char (org-glance-headline:begin headline))
            (org-glance-headline:update (org-glance-headline:from-element (org-element-at-point))
              :buffer b)))))))

(cl-defun org-glance-headline:add-log-note (string &rest objects)
  (org-glance-headline:with-headline-at-point
   (goto-char (org-log-beginning t))
   (insert (apply #'format string objects) "\n")))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance-headline:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance:encrypt-region beg end password))))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance-headline:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance:decrypt-region beg end password))))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
           do (org-with-limited-levels
               (org-map-tree 'org-demote))))

;; Relations

(cl-defun org-glance-headline:hash (&optional (headline (org-glance-headline:at-point)))
  (let ((contents (org-glance-headline-contents headline)))
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (buffer-hash))))

(cl-defun org-glance-headline:repeated-p ()
  (org-glance-datetime-headline-repeated-p))

(cl-defun org-glance-headline:make-directory (location title)
  (abbreviate-file-name
   (make-temp-file
    (org-glance--make-file-directory
     (f-join location
             (concat (format-time-string "%Y-%m-%d_")
                     (->> title
                          (replace-regexp-in-string "[^a-z0-9A-Z_]" "-")
                          (replace-regexp-in-string "\\-+" "-")
                          (replace-regexp-in-string "\\-+$" "")
                          (s-truncate 30))
                     "-")))
    'directory)))

(cl-defun org-glance-headline:overview ()
  "Return HEADLINE high-level usability characteristics."
  (org-glance-headline:with-headline-at-point
   (cl-flet ((org-list (&rest items) (org-glance--join-leading-separator-but-null "\n- " items))
             (org-newline (&rest items) (org-glance--join-leading-separator-but-null "\n" items)))
     (let* ((timestamps (cl-loop for timestamp in (-some->> (org-glance-datetime-headline-timestamps)
                                                    (org-glance-datetime-filter-active)
                                                    (org-glance-datetime-sort-timestamps))
                                 collect (org-element-property :raw-value timestamp)))
            (clocks (org-glance-headline:with-headline-at-point
                     (cl-loop while (re-search-forward org-clock-line-re (point-max) t)
                              collect (buffer-substring-no-properties (pos-bol) (pos-eol)))))
            (relations (org-glance-headline-relations))
            (tags (org-make-tag-string (org-get-tags nil t)))
            (headline (org-glance-headline:at-point))
            (state (org-glance-headline:state headline))
            (id (org-glance-headline:id headline))
            (title (org-glance:headline-title))
            (priority (org-glance-headline:priority headline))
            (closed (org-element-property :closed (org-element-at-point)))
            (schedule (org-glance-headline:schedule headline))
            (deadline (org-glance-headline:deadline headline))
            (encrypted (org-glance-headline:encrypted? headline))
            (linked (org-glance-headline:linked? headline)))
       (with-temp-buffer
         (insert
          (concat
           "* "
           state
           (if (string-empty-p state)
               ""
             " ")
           (if priority
               (concat "[#" (char-to-string priority) "]" " ")
             "")
           title
           " "
           tags
           "\n"

           (if (and closed (listp closed))
               (concat "CLOSED: "
                       (org-element-property :raw-value closed)
                       (if (or schedule deadline)
                           " "
                         ""))
             "")

           (if schedule
               (concat "SCHEDULED: "
                       (org-element-property :raw-value schedule)
                       (if deadline
                           " "
                         ""))
             "")

           (if deadline
               (concat "DEADLINE: " (org-element-property :raw-value deadline))
             "")

           (if (or schedule deadline closed)
               "\n"
             "")

           ":PROPERTIES:\n"
           ":ORG_GLANCE_ID: " id "\n"
           ":DIR: " (abbreviate-file-name default-directory) "\n"
           ":END:"

           (org-glance--join-leading-separator-but-null "\n\n"
             (list

              (when (or encrypted linked)
                (concat "*Features*"
                        (org-list
                         (when encrypted "Encrypted")
                         (when linked "Linked"))))

              (when timestamps
                (concat "*Timestamps*" (apply #'org-list timestamps)))

              (when relations
                (concat "*Relations*" (apply #'org-list (mapcar #'org-glance-relation-interpreter relations))))

              (when clocks
                (concat "*Time spent*" (apply #'org-newline clocks)))))))
         (condition-case nil
             (org-update-checkbox-count-maybe 'all)
           (error nil))
         (buffer-string))))))

(cl-defun org-glance-headline-reference (&optional (type 'org-glance-visit))
  (org-glance-headline:with-headline-at-point
   (let* ((headline (org-glance-headline:at-point))
          (id (org-glance-headline:id headline))
          (state (org-glance-headline:state headline))
          (alias (-some->> (org-glance:headline-alias)
                   (replace-regexp-in-string (format "^%s[[:space:]]*" state) "")))
          (tags (org-glance-headline:tags headline))
          (tag (s-join ", " (cl-loop for tag in tags
                                     collect (format "[[org-glance-overview:%s][%s]]" (downcase tag) tag)))))
     (format "%s%s [[%s:%s][%s]]"
             (if (string-empty-p state) "" (format "[[org-glance-state:%s][%s]] " state state))
             tag
             type
             id
             alias))))

;; Metadata

(cl-defun org-glance-metadata:save (metadata file)
  (declare (indent 1))
  (mkdir (file-name-directory file) 'parents)
  (with-temp-file file
    (insert (prin1-to-string metadata)))
  metadata)

(cl-defun org-glance-metadata:add-headline (headline metadata)
  (puthash (org-glance-headline:id headline)
           (org-glance-headline:serialize headline)
           metadata))

(cl-defun org-glance-metadata:remove-headline (headline metadata)
  (remhash (org-glance-headline:id headline)
           metadata))

(cl-defun org-glance-metadata:create (file &optional headlines)
  "Create metadata from HEADLINES and write it to FILE."
  (unless (f-exists? file)
    (cl-loop with metadata = (make-hash-table :test 'equal)
             for headline in headlines
             do (org-glance-metadata:add-headline headline metadata)
             finally (org-glance-metadata:save metadata file))))

(defun org-glance-metadata:read (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-glance-metadata:headlines (metadata)
  (cl-loop for id being the hash-keys of metadata using (hash-value value)
           collect (-> (org-glance-headline:deserialize value)
                       (org-glance-headline:update :ORG_GLANCE_ID id))))

(cl-defun org-glance-headlines (&key db (scope '(agenda)) (filter #'(lambda (_) t)) (db-init nil))
  "Deprecated method, refactor it."
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (let ((headlines (org-glance-scope-headlines scope filter)))
                        (org-glance-metadata:create db headlines)
                        headlines))
          (load-db?   (org-glance-metadata:headlines (org-glance-metadata:read db)))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

(cl-defun org-glance-metadata:get-headline (id)
  "Get headline by ID."
  (when (symbolp id)
    (setq id (symbol-name id)))

  (cl-loop for tag being the hash-keys of org-glance-tags
           for metadata = (->> tag
                                org-glance:tag-metadata-file-name
                                org-glance-metadata:read)
           for headline = (gethash id metadata)
           when headline
           collect (-> headline
                       (org-glance-headline:deserialize)
                       (org-glance-headline:update :ORG_GLANCE_ID id))
           into result
           finally (return (car result))))

;; TODO refactor, slow
(cl-defun org-glance-all-headlines (&optional filter)
  (cl-loop for tag being the hash-keys of org-glance-tags
           append (cl-loop for headline in (org-glance:tag-headlines tag)
                           when (or (null filter) (funcall filter headline))
                           collect (cons (format "[%s] %s" tag (org-glance:headline-title headline))
                                         (list headline tag)))))

;; TODO refactor, slow
(cl-defun org-glance-metadata:choose-headline (&key (filter #'org-glance-headline:active?))
  "Main retriever, refactor needed."
  (let* ((headlines (org-glance-all-headlines filter))
         (choice (completing-read "Headline: " headlines nil t))
         (headline.tag (alist-get choice headlines nil nil #'string=)))
    (unless headline.tag
      (HEADLINE-NOT-FOUND choice))

    (let ((headline (car headline.tag))
          (tag (cadr headline.tag)))
      (org-glance-headline:with-narrowed-headline headline
        (org-glance-headline:update (org-glance-headline:from-element (org-element-at-point))
                                    :tag tag)))))

(cl-defun org-glance-relation-interpreter (relation)
  ;; please, avoid metadata here
  (org-element-link-interpreter (org-element-property :link relation)
                                (org-element-property :contents relation))

  ;; (org-glance-headline-reference)
  ;; (org-glance-headline:with-narrowed-headline (org-glance-metadata:get-headline (org-element-property :id relation))
  ;;   (let ((ref )
  ;;         (ts (cl-loop for timestamp in (-some->> (org-glance-datetime-headline-timestamps 'include-schedules)
  ;;                                         (org-glance-datetime-filter-active)
  ;;                                         (org-glance-datetime-sort-timestamps))
  ;;                collect (org-element-property :raw-value timestamp))))
  ;;     (if ts
  ;;         (concat ref " on " (car ts))
  ;;       ref)))
  )

(cl-defun org-glance-relation-type-parser ()
  (quote mention))

(cl-defun org-glance-relation-type-interpreter (relation)
  (cl-case (org-element-property :type relation)
    ((subtask quote) "Subtask")
    ((project quote) "Part of a project")
    ((mention quote) "-")
    (t (prin1-to-string (org-element-property :type relation)))))

(cl-defun org-glance-headline-relations ()
  "Get all first-level relations of headline at point."
  (org-glance-headline:with-headline-at-point
   (cl-loop
    with relations = (make-hash-table)
    for link in (org-element-map (org-element-parse-buffer) 'link #'identity)
    for id = (intern (org-element-property :path link))
    for type = (cond
                ((memq (intern (org-element-property :type link)) '(org-glance-visit org-glance-open))
                 (let ((link-type (save-excursion
                                    (goto-char (org-element-property :begin link))
                                    (org-glance-relation-type-parser))))
                   (if (memq link-type '(subtask subtask-done project project-done))
                       ;; actualize link state for subtasks and projects
                       (if-let (headline (org-glance-metadata:get-headline id))
                           (condition-case nil
                               (org-glance-headline:with-narrowed-headline headline
                                 (let ((state (intern (or (org-get-todo-state) "")))
                                       (done-kws (mapcar #'intern org-done-keywords)))
                                   (cond ((memq state done-kws) (cond ((memq link-type '(subtask subtask-done)) 'subtask-done)
                                                                      ((memq link-type '(project project-done)) 'project-done)
                                                                      (t 'subtask-done)))
                                         (t (cond ((memq link-type '(subtask subtask-done)) 'subtask)
                                                  ((memq link-type '(project project-done)) 'project)
                                                  (t 'subtask))))))
                             (HEADLINE-NOT-FOUND link-type))
                         link-type)
                     link-type)))
                (t nil))
    when (and type
              (or (not (gethash id relations))
                  (and (memq type '(subtask subtask-done))
                       (not (memq (org-element-property :type (gethash id relations))
                                  '(subtask subtask-done))))))
    do (let ((relation (list 'org-glance-relation
                             (list :id id
                                   :type type
                                   :contents (condition-case nil
                                                 (buffer-substring-no-properties
                                                  (org-element-property :contents-begin link)
                                                  (org-element-property :contents-end link))
                                               (error nil))
                                   :link link))))
         (puthash id relation relations))
    finally (return (hash-table-values relations)))))

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . org-glance--file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance--agenda-with-archives)))

(cl-defgeneric org-glance-scope (_)
  "Convert input to list of files if possible.")

(cl-defmethod org-glance-scope ((file string))
  "Return list of file S if exists."
  (let ((files (cond
                ((not (file-exists-p file)) (message "File \"%s\" does not exist" file) nil)
                ((not (file-readable-p file)) (message "File \"%s\" is not readable" file) nil)
                ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                ((with-temp-buffer
                   (insert-file-contents file)
                   (hack-local-variables)
                   (alist-get 'org-glance-overview-mode (buffer-local-variables))) (message "File \"%s\" is in `org-glance-overview' mode" file) nil)
                (t (list file)))))
    (cl-loop
     for file in files
     when (member (file-name-extension file) org-glance-scope:extensions)
     collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  "Convert L to flattened list of files."
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  "Return extracted S from `org-glance-scope--default-scope-alist'."
  (if-let (reserved-scope (assoc s org-glance-scope--default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  "Return list of files from buffer B."
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  "Adapt result of F."
  (-some->> f funcall org-glance-scope))

(defun org-glance-scope--choose-headline (choice headlines)
  "Deprecated helper."
  (--first (string= (org-glance:headline-title it) choice) headlines))

(cl-defun org-glance-scope-headlines (scope &optional (filter (lambda (headline) headline)))
  (cl-loop
   for file in (org-glance-scope scope)
   append (-non-nil (mapcar filter (org-glance-headline:extract-from file)))))

(defface org-glance-link-materialize-face
  '((((background dark)) (:inherit default :underline "MediumPurple3"))
    (t (:inherit default :underline "Magenta")))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-overview-face
  '((((background dark)) (:inherit default :slant italic))
    (t (:inherit default :slant italic)))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-state-face
  '((((background dark)) (:inherit default :weight bold))
    (t (:inherit default :weight bold)))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(cl-defun org-glance-link:choose-thing-for-materialization ()
  (concat "org-glance-visit:" (org-glance-headline:id (org-glance-metadata:choose-headline))))

(cl-defun org-glance-link:choose-thing-for-opening ()
  (concat "org-glance-open:" (org-glance-headline:id (org-glance-metadata:choose-headline
                                                      :filter #'(lambda (headline)
                                                                  (and
                                                                   (org-glance-headline:active? headline)
                                                                   (org-glance-headline:linked? headline)))))))


(org-link-set-parameters
 "org-glance-visit"
 :follow #'org-glance-link:materialize
 :face 'org-glance-link-materialize-face
 :complete 'org-glance-link:choose-thing-for-materialization
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(org-link-set-parameters
 "org-glance-open"
 :follow #'org-glance-link:open
 :complete 'org-glance-link:choose-thing-for-opening
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(org-link-set-parameters
 "org-glance-overview"
 :follow #'org-glance-link:overview
 :face 'org-glance-link-overview-face)

(org-link-set-parameters
 "org-glance-state"
 :follow #'org-glance-link:state
 :face 'org-glance-link-state-face)

;; (defcustom org-glance-link:command 'man
;;   "The Emacs command to be used to display a man page."
;;   :group 'org-link
;;   :type '(choice (const man) (const woman)))

(defun org-glance-link:materialize (id &optional _)
  "Materialize org-glance headline identified by ID."
  (org-glance:materialize (org-glance-metadata:get-headline id)))

(defun org-glance-link:open (id &optional _)
  "Open org-glance headline identified by ID."
  (org-glance:open (org-glance-metadata:get-headline id)))

(defun org-glance-link:overview (tag &optional _)
  "Open org-glance headline identified by ID."
  (org-glance-overview (intern (downcase tag))))

(defun org-glance-link:state (_state &optional _)
  "Get all headlines with todo state equal STATE."
  (user-error "Not implemented."))

;; (defun org-glance-link:store-link ()
;;   "Store a link to a man page."
;;   (when (memq major-mode '(Man-mode woman-mode))
;;     ;; This is a man page, we do make this link.
;;     (let* ((page (org-glance-link:get-page-name))
;;            (link (concat "man:" page))
;;            (description (format "Man page for %s" page)))
;;       (org-link-store-props
;;        :type "man"
;;        :link link
;;        :description description))))

;; (defun org-glance-link:get-page-name ()
;;   "Extract the page name from the buffer name."
;;   ;; This works for both `Man-mode' and `woman-mode'.
;;   (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
;;       (match-string 1 (buffer-name))
;;     (error "Cannot create link to this man page")))

;; (defun org-glance-link:export (link description format _)
;;   "Export a man page link from Org files."
;;   (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
;;         (desc (or description link)))
;;     (pcase format
;;       (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
;;       (`latex (format "\\href{%s}{%s}" path desc))
;;       (`texinfo (format "@uref{%s,%s}" path desc))
;;       (`ascii (format "%s (%s)" desc path))
;;       (t path))))

(cl-defun org-glance (&key db
                           default
                           (db-init nil)
                           (filter #'(lambda (_) t))
                           (scope '(agenda))
                           (action #'org-glance-headline:visit)
                           (prompt "Glance: "))
  "Deprecated main method, refactoring needed."
  (let ((headlines (org-glance-headlines :db db
                                         :db-init db-init
                                         :scope scope
                                         :filter filter)))
    (unwind-protect
        (when-let (choice (or default
                              (completing-read prompt (mapcar #'org-glance:headline-title headlines) nil t)))
          (if-let (headline (org-glance-scope--choose-headline choice headlines))
              (condition-case nil
                  (funcall action headline)
                (DB-OUTDATED (message "Metadata %s is outdated, actualizing..." db)
                             (redisplay)
                             (org-glance :scope scope
                                         :filter filter
                                         :action action
                                         :db db
                                         :db-init t
                                         :default choice
                                         :prompt prompt)))
            (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
