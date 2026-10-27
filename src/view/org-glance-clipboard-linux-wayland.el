;;; org-glance-clipboard-linux-wayland.el --- Linux Wayland -*- lexical-binding: t -*-

;;; Commentary:
;; Confidential clipboard support for Linux Wayland.

;;; Code:

(require 'org-glance-clipboard)

(defun org-glance-clipboard-linux-wayland:copy (value)
  "Offer VALUE through wl-copy with a confidentiality hint and expiry."
  (unless (executable-find "wl-copy")
    (user-error "Confidential copying requires wl-clipboard 2.3 or newer"))
  (let ((process (org-glance-clipboard--send
                  value '("wl-copy" "--foreground" "--sensitive"
                          "--type" "text/plain;charset=utf-8"))))
    (run-at-time org-glance-clipboard-ttl nil
                 #'org-glance-clipboard--stop process)))

(provide 'org-glance-clipboard-linux-wayland)
;;; org-glance-clipboard-linux-wayland.el ends here
