;;; test-overview.el --- Tests for the v2 graph-backed overview  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:overview-render ()
  "Overview renders one heading per live headline, with state/tags/planning/id."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha :work:" "SCHEDULED: <2025-01-10 Fri>")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview-v2:render graph)))
      (should (s-contains? "* TODO Alpha" text))
      (should (s-contains? ":work:" text))
      (should (s-contains? "SCHEDULED: <2025-01-10 Fri>" text))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (s-contains? "* Beta" text))
      (should (s-contains? ":ORG_GLANCE_ID: o2" text)))))

(ert-deftest org-glance-test:overview-render-tag-filter ()
  "A tag argument restricts the overview to matching headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview-v2:render graph 'work)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (not (s-contains? ":ORG_GLANCE_ID: o2" text))))))

(ert-deftest org-glance-test:overview-id-at-point ()
  "The headline id under point is read from its ORG_GLANCE_ID property."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (with-temp-buffer
      (insert (org-glance-overview-v2:render graph))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (re-search-forward "Alpha")
      (should (string= "o1" (org-glance-overview-v2:id-at-point))))))

(ert-deftest org-glance-test:overview-materialize-at-point ()
  "Materializing from the overview opens the headline under point."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* TODO Alpha"))
    (let ((org-glance-graph-v2 graph) (opened nil))
      (with-temp-buffer
        (insert (org-glance-overview-v2:render graph))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (re-search-forward "Alpha")
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq opened b) b)))
          (org-glance-overview-v2:materialize)))
      (should (bufferp opened)))))

(ert-deftest org-glance-test:overview-write-file ()
  "The overview is materialized to a file under the store."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview-v2:write graph)))
      (should (f-exists? file))
      (should (s-contains? ":ORG_GLANCE_ID: o1" (f-read-text file 'utf-8))))))

(provide 'test-overview)
;;; test-overview.el ends here
