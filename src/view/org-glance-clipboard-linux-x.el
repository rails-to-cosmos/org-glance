;;; org-glance-clipboard-linux-x.el --- Linux X11 -*- lexical-binding: t -*-

;;; Commentary:
;; Independent clipboard ownership for Linux X11.

;;; Code:

(require 'org-glance-clipboard)

(defun org-glance-clipboard-linux-x:copy (value)
  "Offer VALUE through the independent X11 memory helper."
  (org-glance-clipboard--linux-copy value "x11"))

(provide 'org-glance-clipboard-linux-x)
;;; org-glance-clipboard-linux-x.el ends here
