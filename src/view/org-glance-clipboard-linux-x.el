;;; org-glance-clipboard-linux-x.el --- Linux X11 -*- lexical-binding: t -*-

;;; Commentary:
;; Confidential clipboard support for Linux X11.

;;; Code:

(require 'cl-lib)
(require 'select)
(require 'org-glance-clipboard)

(defun org-glance-clipboard-linux-x--expire (terminal token)
  "Clear TERMINAL's clipboard if it still contains the identical TOKEN."
  (when-let* ((frame (cl-find-if
                     (lambda (frame) (eq terminal (frame-terminal frame)))
                     (frame-list))))
    (with-selected-frame frame
      (when (and (gui-backend-selection-owner-p 'CLIPBOARD)
                 (eq token (gui-get-selection 'CLIPBOARD '_EMACS_INTERNAL)))
        (gui-set-selection 'CLIPBOARD nil)))))

(defun org-glance-clipboard-linux-x--confidential-p (selection _target value)
  "Return non-nil for a confidential clipboard SELECTION carrying VALUE."
  (and (eq selection 'CLIPBOARD) (stringp value)
       (> (length value) 0)
       (get-text-property 0 'org-glance-confidential value)))

(defun org-glance-clipboard-linux-x--hint (selection target value)
  "Return the confidential hint for SELECTION, TARGET and VALUE."
  (when (org-glance-clipboard-linux-x--confidential-p selection target value)
    (cons target (encode-coding-string "secret" 'utf-8-unix))))

(defun org-glance-clipboard-linux-x:copy (value)
  "Offer VALUE on X11's clipboard with a confidentiality hint and expiry."
  (let ((token (propertize value 'org-glance-confidential t)))
    (setf (alist-get 'x-kde-passwordManagerHint selection-converter-alist)
          '(org-glance-clipboard-linux-x--confidential-p
            . org-glance-clipboard-linux-x--hint))
    (gui-set-selection 'CLIPBOARD token)
    (run-at-time org-glance-clipboard-ttl nil
                 #'org-glance-clipboard-linux-x--expire (frame-terminal) token)))

(provide 'org-glance-clipboard-linux-x)
;;; org-glance-clipboard-linux-x.el ends here
