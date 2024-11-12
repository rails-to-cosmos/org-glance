(require 'org-glance)
(require 'with-simulated-input)

(When "^I request \"\\([^\"]+\\)\" overview$"
  (lambda (class)
    (org-glance-overview (intern class))))

(When "^I am in \"\\([^\"]+\\)\" overview buffer$"
  (lambda (category)
    (should (and (string= category (org-get-category))
                 (alist-get 'org-glance-overview-mode (buffer-local-variables))))))

(When "^I jump to the first headline$"
  (lambda ()
    (goto-char (point-min))
    (org-next-visible-heading 1)))

(When "^I kill headline at point$"
  (lambda ()
    (with-simulated-input "y RET y RET"
      (org-glance-overview:kill-headline))))

(And "^I materialize original headline from overview$"
  (lambda ()
    (org-glance-overview:materialize-headline)))
