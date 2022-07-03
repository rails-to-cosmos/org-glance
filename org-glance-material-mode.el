;; -*- lexical-binding: t; -*-

"Materialize headline to apply changes in transaction manner."

(require 'org)
(require 'org-glance-headline)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map)

(defcustom org-glance-before-materialize-sync-hook nil
  "Normal hook that is run before a materialized buffer is synchronized to its origin."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-after-materialize-sync-hook nil
  "Hook that is run after a materialized buffer is synchronized to its origin."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(provide 'org-glance-material-mode)
