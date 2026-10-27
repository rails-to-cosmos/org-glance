;;; org-glance-clipboard-linux-wayland.el --- Linux Wayland -*- lexical-binding: t -*-

;;; Commentary:
;; Independent clipboard ownership for Linux Wayland.

;;; Code:

(require 'org-glance-clipboard)

(defun org-glance-clipboard-linux-wayland:copy (value)
  "Offer VALUE through the independent Wayland memory helper."
  (org-glance-clipboard--linux-copy value "wayland"))

(provide 'org-glance-clipboard-linux-wayland)
;;; org-glance-clipboard-linux-wayland.el ends here
