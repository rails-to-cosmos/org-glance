(defun org-glance-test-class-filter (headline)
  (member (downcase "human") (org-glance-headline-class headline)))

(let ((dst "/tmp/store"))
  (f-delete dst t)
  (setq test-store (org-glance-store:from-scratch dst
                     "* TODO a :Task:
1"
                     "* DONE b :TAsk:"
                     "* COMMENT c
aes-encrypted V 1.3-OCB-B-4-4-M
1/tktn7J+sRqmM2KLefQQZtIYV/FAOcDn+Rs/s5Nm17pNMFtusnXrgrjwzxWFk8F4YSBdCbbRwzl
wUVErGnLFnK5LJ17kYnL18iRTAGhEhUQqyxXqB3DQ/41"
                     "* COMMENT a
2"))

  (setq test-view (org-glance-store:view test-store #'org-glance-headline-header-p))

  (org-glance-view:materialize test-view (f-join dst "main.org"))

  ;; emulate source corruption
  (append-to-file "* d" nil (f-join dst "main.org"))
  )
