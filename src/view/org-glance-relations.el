;; -*- lexical-binding: t -*-

;;; org-glance-relations.el --- material↔table relations bridge

;;; Commentary:
;; Sits above material and table; material never requires table (invariant 31).

;;; Code:

(require 'org-glance-material)
(require 'org-glance-table)

(cl-defun org-glance-relations:references ()
  "Open the relation table of this headline, both directions (`C-c @').
Relations read LAST-SAVED metadata; save first to see this session's edges."
  (interactive)
  (org-glance-material--ensure)
  (org-glance-table:visit-relations org-glance-material--graph
                                    org-glance-material--id))

(define-key org-glance-material-mode-map (kbd "C-c @") #'org-glance-relations:references)

(provide 'org-glance-relations)
;;; org-glance-relations.el ends here
