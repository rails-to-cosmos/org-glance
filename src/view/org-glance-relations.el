;; -*- lexical-binding: t -*-

;;; org-glance-relations.el --- material↔table relations bridge

;;; Commentary:
;; The one UI seam that spans both the material model and the table
;; projection: opening a material headline's relation table.  It sits ABOVE
;; both, so it may require each outright — the edge that would otherwise force
;; material to reach up into table (invariant 31).

;;; Code:

(require 'org-glance-material)
(require 'org-glance-table)

(cl-defun org-glance-relations:references ()
  "Open the table of every headline this one relates to (`C-c @').
Rows are this headline's edge targets and its referrers, each row's direction
and kind in the `Relation' column (`org-glance-table:visit-relations').
Relations read LAST-SAVED metadata, so save first to see this session's edges."
  (interactive)
  (org-glance-material--ensure)
  (org-glance-table:visit-relations org-glance-material--graph
                                    org-glance-material--id))

(define-key org-glance-material-mode-map (kbd "C-c @") #'org-glance-relations:references)

(provide 'org-glance-relations)
;;; org-glance-relations.el ends here
