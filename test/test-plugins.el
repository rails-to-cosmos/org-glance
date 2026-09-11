;;; test-plugins.el --- Tests for the plugin loader  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:plugins-loader-is-demoted ()
  "`org-glance--load-plugins' requires each listed plugin.
An unknown or broken one is skipped without breaking init (invariant 9)."
  (let ((org-glance-plugins '(no-such-plugin-xyz)))
    (org-glance--load-plugins))                    ; must not signal
  (let* ((loaded nil)
         (org-glance-plugins '(fake)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (setq loaded feature))))
      (org-glance--load-plugins))
    (should (eq 'org-glance-fake loaded))))

(ert-deftest org-glance-test:plugin-enable-reports-missing-package ()
  "Enabling a plugin whose package is absent errors and records nothing."
  (let ((org-glance-plugins nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "nope-xyz")))
      (should-error (call-interactively #'org-glance-plugin-enable) :type 'user-error))
    (should-not org-glance-plugins)))

(ert-deftest org-glance-test:plugin-enable-records-and-loads ()
  "A present plugin loads and joins `org-glance-plugins'; batch skips saving."
  (let ((org-glance-plugins nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fake"))
              ((symbol-function 'require) (lambda (&rest _) t)))
      (call-interactively #'org-glance-plugin-enable))
    (should (equal '(fake) org-glance-plugins))))

(ert-deftest org-glance-test:plugin-enable-nothing-left-to-offer ()
  "With every known plugin already enabled, `I' reports that and never prompts."
  (let ((org-glance-plugins (copy-sequence org-glance-plugins-available))
        (prompted nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompted t) "")))
      (should-error (call-interactively #'org-glance-plugin-enable) :type 'user-error))
    (should-not prompted)))

(ert-deftest org-glance-test:plugin-disable ()
  "`U' drops the chosen plugin from `org-glance-plugins'; batch skips saving.
With none enabled it reports so and never prompts."
  (let ((org-glance-plugins '(fake other)))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fake")))
      (call-interactively #'org-glance-plugin-disable))
    (should (equal '(other) org-glance-plugins)))
  (let ((org-glance-plugins nil)
        (prompted nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompted t) "")))
      (should-error (call-interactively #'org-glance-plugin-disable) :type 'user-error))
    (should-not prompted)))

(ert-deftest org-glance-test:transient-plugins-description ()
  "The System heading names enabled plugins and marks those that failed to load.
It is the only place a demoted load failure shows (invariant 9)."
  (let ((org-glance-plugins nil))
    (should (s-contains? "none" (org-glance-transient--plugins-description))))
  (let ((org-glance-plugins '(no-such-plugin-xyz)))
    (should (s-contains? "no-such-plugin-xyz (not loaded)"
                         (org-glance-transient--plugins-description))))
  (let ((org-glance-plugins '(fake)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f &rest _) (eq f 'org-glance-fake))))
      (let ((text (org-glance-transient--plugins-description)))
        (should (s-contains? "fake" text))
        (should-not (s-contains? "not loaded" text))))))

(provide 'test-plugins)
;;; test-plugins.el ends here
