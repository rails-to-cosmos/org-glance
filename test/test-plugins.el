;;; test-plugins.el --- Tests for the plugin loader  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:plugins-loader-is-demoted ()
  "`org-glance--load-plugins' requires each listed plugin, and an unknown or
broken one is skipped rather than breaking init (invariant 9)."
  (let ((org-glance-plugins '(no-such-plugin-xyz)))
    (org-glance--load-plugins))                    ; must not signal
  (let* ((loaded nil)
         (org-glance-plugins '(fake)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (setq loaded feature))))
      (org-glance--load-plugins))
    (should (eq 'org-glance-fake loaded))))

(ert-deftest org-glance-test:plugin-install-reports-missing-package ()
  "Each plugin is a separate PACKAGE: installing one whose library is absent
says so loudly and records nothing."
  (let ((org-glance-plugins nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "nope-xyz")))
      (should-error (call-interactively #'org-glance-plugin-install) :type 'user-error))
    (should-not org-glance-plugins)))

(ert-deftest org-glance-test:plugin-install-records-and-loads ()
  "A plugin whose library IS present loads and lands in `org-glance-plugins'
\(batch skips the customize-save)."
  (let ((org-glance-plugins nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fake"))
              ((symbol-function 'require) (lambda (&rest _) t)))
      (call-interactively #'org-glance-plugin-install))
    (should (equal '(fake) org-glance-plugins))))

(provide 'test-plugins)
;;; test-plugins.el ends here
