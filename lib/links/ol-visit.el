;;; ol-visit.el - Support for links to org-glance headliens
(require 'org-glance-module)
(require 'ol)

(org-link-set-parameters
 "org-glance-visit"
 :follow #'org-glance-link:open
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

;; (defcustom org-glance-link:command 'man
;;   "The Emacs command to be used to display a man page."
;;   :group 'org-link
;;   :type '(choice (const man) (const woman)))

(defun org-glance-link:open (id _)
  "Visit org-glance headline identified by ID."
  (org-glance-headline:visit id))

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

(org-glance-module-provide)
;;; ol-visit.el ends here
