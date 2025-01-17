;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)
(require 'org)

(require 'org-glance-headline1)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                (:conc-name org-glance-graph:))
  (mutex (make-mutex) :read-only t :type mutex)
  (directory org-glance-directory :read-only t :type directory))

(cl-defmacro org-glance-graph:mutate (graph &rest forms)
  (declare (indent 1))
  `(progn
     (cl-check-type ,graph org-glance-graph)
     (mutex-lock (org-glance-graph:mutex ,graph))
     (unwind-protect ,@forms
       (mutex-unlock (org-glance-graph:mutex ,graph)))))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let ((graph (make-org-glance-graph :directory directory)))
    (f-mkdir-full-path (org-glance-graph:data-path graph))
    (f-mkdir-full-path (org-glance-graph:meta-path graph))
    graph))

(cl-defun org-glance-graph:id-path (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (f-join (org-glance-graph:data-path graph) (substring id 0 2) (substring id 2)))

(cl-defun org-glance-graph:data-path (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "data"))

(cl-defun org-glance-graph:meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "meta"))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:mutate graph
    (cl-loop while t
             for id = (org-id-uuid)
             for id-path = (org-glance-graph:id-path graph id)
             unless (f-exists? id-path)
             return (progn (f-mkdir-full-path id-path) id))))

(cl-defun org-glance-graph:add-headline (graph headline)
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline1)
  (let* ((id (org-glance-graph:make-id graph))
         (data (list :id id
                     :title (org-glance-headline1:title headline)
                     :hash (org-glance-headline1:hash headline)
                     :relations nil))
         (json (json-serialize data)))
    (org-glance-graph:mutate graph
      (f-append-text (format "%s\n" json) `utf-8 (f-join (org-glance-graph:meta-path graph) "id"))
      (with-temp-file (f-join (org-glance-graph:id-path graph id) "data.org")
        (insert (org-glance-headline1:contents headline))))
    id))

(cl-defun org-glance-graph:remove-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:get-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:add-relation (graph relation a b)
  (cl-check-type graph org-glance-graph)
  (cl-check-type relation string)
  (cl-check-type a string)
  (cl-check-type b string)
  (let ((data (json-serialize (vector relation a b)))
        (encoding 'utf-8)
        (file (f-join (org-glance-graph:meta-path graph) "relations") ))
    (org-glance-graph:mutate graph
      (f-append-text (format "%s\n" data) encoding file))))

(cl-defmacro org-glance-jsonl:--iter-lines (file &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (cl-loop with window-size = 1000
              with file-size = (file-attribute-size (file-attributes ,file))
              for upper-bound downfrom file-size downto 0 by window-size
              for lower-bound = (max 0 (- upper-bound window-size))
              do (progn (goto-char (point-min))
                        (insert-file-contents-literally ,file nil lower-bound upper-bound)
                        (goto-char (point-max))
                        (skip-chars-backward "\n")
                        (beginning-of-line)
                        (while (not (bobp))
                          (let ((it (json-parse-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)) :object-type 'plist)))
                            ,@forms)
                          (delete-line)
                          (forward-line -1)
                          (beginning-of-line)))
              finally (let ((it (json-parse-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)) :object-type 'plist)))
                        ,@forms))))

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (foo (org-glance-headline1--from-lines "* \"foo\"" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]"))
;;        (bar (org-glance-headline1--from-lines "* bar" "123"))
;;        (foo-id (org-glance-graph:add-headline graph foo))
;;        (bar-id (org-glance-graph:add-headline graph bar)))
;;   (org-glance-graph:add-relation graph "parent" foo-id bar-id))

;; (with-temp-buffer
;;   (insert-file-contents-literally "/tmp/glance/meta/id" nil 0 1024)
;;   ;; (buffer-substring-no-properties (point-min) (point-max))
;;   )

(org-glance-jsonl:--iter-lines "/tmp/glance/meta/id"
  (message (prin1-to-string it))
  ;; (let ((id it ;; (plist-get it :title)
  ;;           ))
  ;;   (message id))
  )

(provide 'org-glance-graph)
