;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org-glance-utils)

(cl-deftype org-glance-namespace ()
  '(satisfies org-glance--valid-directory?))

(provide 'org-glance-namespace)
