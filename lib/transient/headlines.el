(require 'org-glance-module)

(org-glance:require
  transient
  lib.core.view
  lib.transient.base)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-overview:agenda*)
    ("o" "Overview" org-glance-overview)]]
  ["Headline actions"
   [("+" "Capture" org-glance-overview:capture)
    ("e" "Extract" org-glance-action-extract)
    ("j" "Jump" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)]]
  (interactive)
  (cl-loop
     for view-dir-name in (directory-files org-glance-directory nil "^[[:word:]]+")
     unless (alist-get view-dir-name -org-glance-initialized-views nil nil #'string=)
     do (let ((view-config-file (org-glance-overview:location view-dir-name "config.json")))
          (when (file-exists-p view-config-file)
            (apply 'org-glance-def-view
                   (cl-loop
                      for (k . v) in (json-read-file view-config-file)
                      for pk = (intern (org-glance:format ":${k}"))
                      for pv = (cond ((member k '(type)) (mapcar 'intern v))
                                     (t (intern v)))
                      when pk
                      append (list pk pv)))
            (push (cons view-dir-name (current-time)) -org-glance-initialized-views))))
  (transient-setup 'org-glance-form-action))

;; migration script
;; (cl-loop
;;    for id being the hash-keys of org-glance:views using (hash-value view)
;;    for config = (list (cons 'id (org-glance-view-id view))
;;                       (cons 'type (org-glance-view-type view)))
;;    for file = (org-glance-overview:location id "config.json")
;;    do (with-temp-file file
;;         (insert (json-encode-alist config))
;;         (json-pretty-print-buffer)))

(org-glance:provide)
