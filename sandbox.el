(let ((dst "/tmp/store"))
  (progn ;; reload glance
    (mapc #'load-file (--filter (and (s-ends-with-p ".el" it) (s-contains-p "org-glance-" it) (not (s-contains-p "org-glance-pkg.el" it))) (f-files ".")))
    (clrhash org-glance-stores)
    (clrhash org-glance-materializations))

  (f-delete dst t)

  (defvar test-store)
  (defvar test-view)

  (setq test-store (org-glance-store:from-scratch dst
                     "* TODO a :Task:
1"
                     "* DONE b :TAsk:"
                     "* COMMENT c
aes-encrypted V 1.3-OCB-B-4-4-M
1/tktn7J+sRqmM2KLefQQZtIYV/FAOcDn+Rs/s5Nm17pNMFtusnXrgrjwzxWFk8F4YSBdCbbRwzl
wUVErGnLFnK5LJ17kYnL18iRTAGhEhUQqyxXqB3DQ/41"
                     "* COMMENT d
2"))

  (setq test-view (org-glance-store:view test-store "Task"))
  (org-glance-view:materialize test-view (f-join dst "main.org"))

  ;; emulate source corruption
  (append-to-file "* d" nil (f-join dst "main.org"))

  (find-file "/tmp/store")
  )

;; (cl-assert (> (org-glance-event-offset (car (org-glance-store:events test-store)))
;;               (org-glance-event-offset (car (last (org-glance-store:events test-store))))))

(let ((res nil))
  (push 1 res)
  (push 2 res)
  res)
