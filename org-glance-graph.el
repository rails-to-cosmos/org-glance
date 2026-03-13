;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)
(require 'org)

(require 'org-glance-utils)
(require 'org-glance-headline)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(define-hash-table-test 'org-glance-graph:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 a)))

(defvar org-glance-graph:list (make-hash-table :test 'org-glance-graph:test)
  "Registered instances of `org-glance-graph' in current session.")

(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                 (:conc-name org-glance-graph:))
  (mutex (make-mutex) :read-only t :type mutex)
  (directory org-glance-directory :read-only t :type directory))

(cl-defstruct (org-glance-headline-metadata (:predicate org-glance-headline-metadata?)
                                             (:conc-name org-glance-headline-metadata:))
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number))

(cl-defun org-glance-headline:metadata (headline)
  (cl-check-type headline org-glance-headline)
  (make-org-glance-headline-metadata
   :id (org-glance-headline:id headline)
   :state (org-glance-headline:state headline)
   :title (org-glance-headline:title headline)
   :tags (org-glance-headline:tags headline)
   :hash (org-glance-headline:hash headline)
   :schedule (org-glance-headline:schedule headline)
   :deadline (org-glance-headline:deadline headline)
   :priority (org-glance-headline:priority headline)))

(cl-defun org-glance-headline:metadata* (obj)
  "Generic variant: return metadata from headline or pass through metadata."
  (cl-typecase obj
    (org-glance-headline-metadata obj)
    (org-glance-headline (org-glance-headline:metadata obj))))

(cl-defun org-glance-headline-metadata:serialize (metadata)
  (cl-check-type metadata org-glance-headline-metadata)
  (list :id (org-glance-headline-metadata:id metadata)
        :state (org-glance-headline-metadata:state metadata)
        :title (org-glance-headline-metadata:title metadata)
        :tags (->> (org-glance-headline-metadata:tags metadata)
                   (mapcar (-partial #'format "%s"))
                   (apply #'vector))
        :hash (org-glance-headline-metadata:hash metadata)
        :schedule (org-glance-headline-metadata:schedule metadata)
        :deadline (org-glance-headline-metadata:deadline metadata)
        :priority (org-glance-headline-metadata:priority metadata)))

(cl-defun org-glance-headline-metadata:serialize* (obj)
  "Generic variant of serialize."
  (cl-typecase obj
    (org-glance-headline-metadata (org-glance-headline-metadata:serialize obj))
    (list obj)
    (t (error "Unable to determine object spec: %s" (prin1-to-string obj)))))

(cl-defun org-glance-headline-metadata:deserialize (data)
  (cl-check-type data list)
  (make-org-glance-headline-metadata :id (plist-get data :id)
                                      :state (plist-get data :state)
                                      :title (plist-get data :title)
                                      :tags (plist-get data :tags)
                                      :hash (plist-get data :hash)
                                      :schedule (plist-get data :schedule)
                                      :deadline (plist-get data :deadline)
                                      :priority (plist-get data :priority)))

(cl-defun org-glance-headline-metadata:active? (metadata)
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((state (or (org-glance-headline-metadata:state metadata) "")))
    (not (member state org-done-keywords))))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let* ((directory (-> directory (file-truename) (f-full)))
         (graph (gethash directory org-glance-graph:list)))
    (unless graph
      (setq graph (make-org-glance-graph :directory directory))
      (f-mkdir-full-path (org-glance-graph:data-path graph))
      (f-mkdir-full-path (org-glance-graph:meta-path graph))
      (f-touch (org-glance-graph:headline-meta-path graph))
      (puthash directory graph org-glance-graph:list))
    graph))

(cl-defmacro org-glance-graph:lock (graph &rest forms)
  "Modify GRAPH data with FORMS."
  (declare (indent 1))
  `(with-mutex (org-glance-graph:mutex ,graph)
     ,@forms))

(cl-defun org-glance-graph:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (org-glance-graph:lock graph
    (cl-loop for spec in meta
             collect (-> spec
                         (org-glance-headline-metadata:serialize*)
                         (json-serialize))
             into result
             finally (f-append-text (concat (s-join "\n" result) "\n") `utf-8 (org-glance-graph:headline-meta-path graph)))))

(cl-defun org-glance-graph:data-path (graph)
  (cl-check-type graph org-glance-graph)
  (-> (f-join (org-glance-graph:directory graph) "data")
      (file-truename)))

(cl-defun org-glance-graph:meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (-> graph
      (org-glance-graph:directory)
      (f-join "meta")
      (file-truename)))

(cl-defun org-glance-graph:headline-meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (-> graph
      (org-glance-graph:meta-path)
      (f-join "headlines.jsonl")
      (file-truename)))

(cl-defun org-glance-graph:headline-data-path (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (-> (f-join (org-glance-graph:data-path graph) (substring id 0 2) (substring id 2))
      (file-truename)))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:lock graph
    (cl-loop while t
             for id = (org-id-uuid)
             for data-path = (org-glance-graph:headline-data-path graph id)
             unless (f-exists? data-path)
             return (prog1 id (f-mkdir-full-path data-path)))))

(cl-defun org-glance-graph:add (graph &rest headlines)
  "Add HEADLINES to GRAPH."
  (cl-check-type graph org-glance-graph)
  (cl-loop for headline in headlines
           collect (org-glance-headline:metadata* headline) into specs
           finally (org-glance-graph:insert graph specs)))

(cl-defun org-glance-graph:get-headline (graph id)
  "Get headline metadata by ID from GRAPH. Returns most recent non-tombstoned entry."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (org-glance-jsonl:iterate (org-glance-graph:headline-meta-path graph)
    (when (string= (plist-get it :id) id)
      (if (not (eq :false (or (plist-get it :tombstone) :false)))
          'tombstone
        (org-glance-headline-metadata:deserialize it)))))

(cl-defun org-glance-graph:delete (graph id)
  "Mark headline ID as tombstoned in GRAPH."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (org-glance-graph:insert graph
    (list (list :id id :tombstone t))))

(cl-defun org-glance-graph:headlines (graph)
  "Return list of all headline metadata in GRAPH (most recent per id, excluding tombstones)."
  (cl-check-type graph org-glance-graph)
  (let ((result (make-hash-table :test 'equal))
        (meta-path (org-glance-graph:headline-meta-path graph)))
    (when (and (f-exists? meta-path) (> (or (file-attribute-size (file-attributes meta-path)) 0) 0))
      (with-temp-buffer
        (insert-file-contents-literally meta-path)
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case nil
              (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     (data (json-parse-string line :object-type 'plist))
                     (id (plist-get data :id)))
                (when id
                  (if (not (eq :false (or (plist-get data :tombstone) :false)))
                      (remhash id result)
                    (puthash id (org-glance-headline-metadata:deserialize data) result))))
            (json-end-of-file nil)
            (error nil))
          (forward-line 1))))
    (hash-table-values result)))

(cl-defun org-glance-graph:headlines-by-tag (graph tag)
  "Return headline metadata from GRAPH filtered by TAG."
  (cl-check-type graph org-glance-graph)
  (cl-check-type tag symbol)
  (let ((tag-name (symbol-name tag)))
    (cl-remove-if-not
     (lambda (meta)
       (cl-some (lambda (t_) (string= (downcase (format "%s" t_)) tag-name))
                (append (org-glance-headline-metadata:tags meta) nil)))
     (org-glance-graph:headlines graph))))

(cl-defun org-glance-graph:completing-read (graph &key (filter nil) (prompt "Headline: "))
  "Interactive headline selection from GRAPH."
  (cl-check-type graph org-glance-graph)
  (let* ((all-meta (org-glance-graph:headlines graph))
         (filtered (if filter (cl-remove-if-not filter all-meta) all-meta))
         (candidates (cl-loop for meta in filtered
                              for title = (or (org-glance-headline-metadata:title meta) "")
                              collect (cons title meta)))
         (choice (completing-read prompt candidates nil t)))
    (alist-get choice candidates nil nil #'string=)))

(cl-defun org-glance-graph:store-headline (graph headline)
  "Write HEADLINE contents to data/{id-prefix}/{id}/headline.org in GRAPH."
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline)
  (let* ((id (org-glance-headline:id headline))
         (data-path (org-glance-graph:headline-data-path graph id))
         (file (f-join data-path "headline.org")))
    (f-mkdir-full-path data-path)
    (f-write-text (org-glance-headline:contents headline) 'utf-8 file)
    file))

(cl-defun org-glance-graph:load-headline (graph id)
  "Read headline contents from data path in GRAPH for ID, return `org-glance-headline'."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let* ((data-path (org-glance-graph:headline-data-path graph id))
         (file (f-join data-path "headline.org")))
    (unless (f-exists? file)
      (error "Headline data not found for id %s at %s" id file))
    (let ((contents (f-read-text file 'utf-8)))
      (org-glance-headline--from-string contents))))

(cl-defun org-glance-graph:capture-buffer (&optional (buffer (current-buffer)))
  (interactive)
  (cl-check-type buffer buffer)
  (cl-loop with elements = (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
           for element in elements
           collect (org-glance-headline--from-element element)))

(provide 'org-glance-graph)
