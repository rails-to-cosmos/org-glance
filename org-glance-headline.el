;; -*- lexical-binding: t -*-

(require 'dash)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'thunk)

(require 'org-glance-exception)
(require 'org-glance-utils)
(require 'org-glance-tag)

(cl-defstruct (org-glance-headline1 (:predicate org-glance-headline1?)
                                    (:conc-name org-glance-headline1:))
  (id nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (title nil :read-only t :type string)
  (contents nil :read-only t :type string)
  (hash nil :read-only t :type string)

  (archived? nil :read-only t :type bool)
  (closed? nil :read-only t :type bool)
  (commented? nil :read-only t :type bool)

  ;; lazy attributes start with "-"
  (-encrypted? nil :read-only t :type bool)
  (-links nil :read-only t :type list)
  (-properties nil :read-only t :type list))

(cl-defun org-glance-headline1:encrypted? (headline) (thunk-force (org-glance-headline1:-encrypted? headline)))
(cl-defun org-glance-headline1:links (headline) (thunk-force (org-glance-headline1:-links headline)))
(cl-defun org-glance-headline1:properties (headline) (thunk-force (org-glance-headline1:-properties headline)))

(cl-defun org-glance-headline1:from-element (element)
  (let ((buffer (org-element-property :buffer element))
        (begin (org-element-property :begin element))
        (end (org-element-property :end element))
        (id (org-element-property :ORG_GLANCE_ID element))
        (tags (mapcar #'org-glance-tag:from-string (org-element-property :tags element)))
        (archived? (not (null (org-element-property :archivedp element))))
        (commented? (not (null (org-element-property :commentedp element))))
        (closed? (not (null (org-element-property :closed element))))
        (title (or (org-element-property :TITLE element)
                   (org-element-property :raw-value element)
                   "")))
    (cl-destructuring-bind (contents hash)
        (with-current-buffer buffer
          (save-restriction
            (narrow-to-region begin end)
            (list (buffer-substring-no-properties (point-min) (point-max)) (buffer-hash))))
      (make-org-glance-headline1
       :id id
       :title title
       :tags tags
       :hash hash
       :contents (org-glance--encode-string contents)
       :archived? archived?
       :commented? commented?
       :closed? closed?
       :-links (thunk-delay (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (org-glance--parse-links)))
       :-properties (thunk-delay (with-temp-buffer
                                   (org-mode)
                                   (insert contents)
                                   (org-glance--buffer-key-value-pairs)))
       :-encrypted? (thunk-delay (with-temp-buffer
                                   (org-mode)
                                   (insert contents)
                                   (goto-char (point-min))
                                   (org-end-of-meta-data t)
                                   (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")))))))))

(cl-defun org-glance-headline1:at-point ()
  (save-excursion
    (org-glance--back-to-heading)
    (cl-do ((element (org-element-at-point) (org-element-at-point)))
        ;; until:
        ((or (and (listp element) (eq (car element) 'headline)) (org-before-first-heading-p) (bobp))
         ;; return:
         (if (and (listp element) (eq (car element) 'headline))
             (org-glance-headline1:from-element element)
           nil))
      ;; do:
      (org-up-heading-or-point-min))))

(org-glance-exception:define org-glance-headline:not-found! "Headline not found")
(org-glance-exception:define org-glance-headline:metadata-corrupted! "Headline metadata corrupted, please reread")

(cl-defun org-glance-headline? (headline) (and (listp headline) (eq (car headline) 'headline)))
(cl-deftype org-glance-headline () '(satisfies org-glance-headline?))

(cl-defun org-glance-headline-metadata? (headline-metadata) (and (listp headline-metadata) (eq (car headline-metadata) 'headline-metadata)))
(cl-deftype org-glance-headline-metadata () '(satisfies org-glance-headline-metadata?))

(defvar org-glance:key-value-pair-re)

(declare-function org-glance--back-to-heading "org-glance-utils.el")
(declare-function org-glance--parse-links "org-glance-utils.el")
(declare-function org-glance--with-file-visited "org-glance-utils.el")
(declare-function org-glance-headline:not-found! "org-glance-exceptions.el")
(declare-function org-glance-tag:from-string "org-glance-tag.el" (value))

(defconst org-glance-headline:spec `((:raw-value   . (:reader org-glance-headline:plain-title  :writer org-glance-headline:plain-title))
                                     (:begin       . (:reader org-glance-headline:begin        :writer org-glance-headline:begin))
                                     (:file        . (:reader org-glance-headline:file-name    :writer org-glance-headline:file-name))
                                     (:commentedp  . (:reader org-glance-headline:commented?   :writer org-glance-headline:commented?))
                                     (:archivedp   . (:reader org-glance-headline:archived?    :writer org-glance-headline:archived?))
                                     (:linked      . (:reader org-glance-headline:linked?      :writer org-glance-element:linked?))
                                     (:propertized . (:reader org-glance-headline:propertized? :writer org-glance-element:propertized?))
                                     (:encrypted   . (:reader org-glance-headline:encrypted?   :writer org-glance-element:encrypted?))
                                     (:buffer      . (:reader org-glance-headline:buffer       :writer org-glance-element:buffer))
                                     (:closed      . (:reader org-glance-headline:closed?      :writer org-glance-headline:closed?)))
  "Map `org-element-property' to `org-glance' extractor method.")

(cl-defun org-glance-headline:at-point ()
  "Search for the first occurence of `org-glance-headline' in parent headlines."
  (when-let (headline (save-excursion (org-glance-headline:search-parents)))
    (setq headline (org-element-put-property headline :buffer (current-buffer)))
    (setq headline (org-element-put-property headline :file (buffer-file-name)))
    (let ((contents (buffer-substring-no-properties (org-element-property :begin headline) (org-element-property :end headline))))
      (setq headline (org-element-put-property headline :contents (org-glance--encode-string contents))))
    headline))

(cl-defmacro org-glance-headline:with-element-narrowing (element &rest forms)
  (declare (indent 1) (debug t))
  `(save-excursion
     (save-restriction
       (goto-char (org-element-property :begin element))
       (org-narrow-to-element)
       ,@forms)))

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  `(save-excursion
     (org-glance-headline:search-parents)
     (unless (org-glance-headline? (org-glance-headline:at-point))
       (error "Unable to find headline at point"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(cl-defmacro org-glance-headline:with-narrowed-headline (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (cl-check-type ,headline org-glance-headline)
     (org-mode)
     (insert (org-glance-headline:contents ,headline))
     (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
       ,@forms)))

(cl-defun org-glance-headline:buffer-positions (id)
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (element) (when (string= (org-glance-headline:id element) id)
                   (org-element-property :begin element)))))

(cl-defun org-glance-headline:search-buffer-by-id (id)
  (let ((positions (org-glance-headline:buffer-positions id)))
    (unless positions
      (org-glance-headline:not-found! "Headline %s not found in file %s" id (buffer-file-name)))

    (when (> (length positions) 1)
      (error "Headline %s is not unique in file %s" id (buffer-file-name)))

    (goto-char (car positions))
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline:update (element &rest properties)
  "Enrich `org-element' ELEMENT with PROPERTIES."
  (declare (indent 1))
  (cl-loop for (key value) on properties by #'cddr
           do (org-element-put-property element key value)
           finally (return element)))

(cl-defun org-glance-headline:serialize (headline)
  (cl-loop for (_ . methods) in org-glance-headline:spec
           for reader = (plist-get methods :reader)
           collect (funcall reader headline)))

(cl-defun org-glance-headline:deserialize (id value)
  (cl-loop with element = (-> (org-element-create 'headline-metadata)
                              (org-glance-headline:update :ORG_GLANCE_ID id))
           for (property . _) in org-glance-headline:spec
           for index from 0
           do (org-glance-headline:update element property (nth index value))
           finally (return element)))

(cl-defun org-glance-element:linked? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (not (null (org-glance--parse-links))))))

(cl-defun org-glance-element:propertized? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (org-end-of-meta-data t)
         (not (null (re-search-forward org-glance:key-value-pair-re nil t))))))

(cl-defun org-glance-element:encrypted? (element)
  (and (org-glance-headline? element)
       (org-glance-headline:with-element-narrowing element
         (org-end-of-meta-data t)
         (not (null (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n"))))))

(cl-defun org-glance-element:buffer (element)
  (when-let (headline (org-glance-headline? element))
    (condition-case nil
        (buffer-name (get-file-buffer (org-glance-headline:file-name headline)))
      (wrong-type-argument nil))))

(cl-defun org-glance-headline:title (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:begin (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:end (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (org-element-property :contents-end headline))

(cl-defun org-glance-headline:tags (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (mapcar #'org-glance-tag:from-string (org-element-property :tags headline)))

(cl-defun org-glance-headline:encrypted? (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (not (null (org-element-property :encrypted headline))))

(cl-defun org-glance-headline:linked? (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (not (null (org-element-property :linked headline))))

(cl-defun org-glance-headline:propertized? (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (not (null (org-element-property :propertized headline))))

(cl-defun org-glance-headline:active? (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (and (not (org-glance-headline:done? headline))
       (not (org-glance-headline:commented? headline))
       (not (org-glance-headline:archived? headline))
       (not (org-glance-headline:closed? headline))))

(cl-defun org-glance-headline:id (headline)
  "Return unique identifer of HEADLINE."
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (headline)
  (substring-no-properties (or (org-element-property :todo-keyword headline) "")))

(cl-defun org-glance-headline:commented? (headline)
  (not (null (org-element-property :commentedp headline))))

(cl-defun org-glance-headline:archived? (headline)
  (not (null (org-element-property :archivedp headline))))

(cl-defun org-glance-headline:closed? (headline)
  (not (null (org-element-property :closed headline))))

(cl-defun org-glance-headline:done? (headline)
  (member (org-glance-headline:state headline) org-done-keywords-for-agenda))

(cl-defun org-glance-headline:alias (headline)
  "Get title of HEADLINE considering alias property."
  (or (org-element-property :ALIAS headline)
      (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)
      ""))

(cl-defun org-glance-headline:priority (headline)
  (org-element-property :priority headline))

(cl-defun org-glance-headline:creation-time (headline)
  (org-element-property :ORG_GLANCE_CREATION_TIME headline))

(cl-defun org-glance-headline:file-name (headline)
  (when-let (file (if (plist-member (nth 1 headline) :file)
                      (org-element-property :file headline)
                    (buffer-file-name)))
    (abbreviate-file-name file)))

(cl-defun org-glance-headline:modtime (headline)
  (-some-> headline
    org-glance-headline:file-name
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:level (headline)
  (or (org-element-property :level headline) 1))

(cl-defun org-glance-headline:buffer (headline)
  (let ((buffer (org-element-property :buffer headline)))
    (cond ((bufferp buffer) (buffer-name buffer))
          (t buffer))))

(cl-defun org-glance-headline:schedule (headline)
  (org-element-property :scheduled headline))

(cl-defun org-glance-headline:deadline (headline)
  (org-element-property :deadline headline))

;; TODO check it for correctness
(cl-defun org-glance-headline:search-parents ()
  "Traverse parents in search of a proper `org-glance-headline'."
  (org-glance--back-to-heading)
  (cl-do ((element (org-element-at-point) (org-element-at-point)))
      ;; until:
      ((or (org-glance-headline? element) (org-before-first-heading-p) (bobp))
       ;; return:
       (if (org-glance-headline? element)
           (org-glance-headline:from-element element)
         nil))
    ;; body:
    (org-up-heading-or-point-min)))

(cl-defun org-glance-headline:from-element (element)
  (when (eql 'headline (org-element-type element))
    (cl-loop for (property . methods) in org-glance-headline:spec
             for index from 0
             for writer = (plist-get methods :writer)
             for value = (funcall writer element)
             do (org-glance-headline:update element property value)
             finally (return element))))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:promote-to-the-first-level ()
  (org-glance--back-to-heading)
  (while (and (org-glance-headline? (org-glance-headline:at-point)) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:replace-headline-at-point (contents)
  (let ((beg (org-glance-headline:begin (org-glance-headline:at-point)))
        (end (save-excursion (org-end-of-subtree t)))
        (inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert contents)))

(cl-defun org-glance-headline:contents (headline)
  (cl-check-type headline org-glance-headline)
  (org-glance--decode-string (org-element-property :contents headline)))

(cl-defun org-glance-headline:hash (headline)
  (cl-check-type headline org-glance-headline)
  (with-temp-buffer
    (insert (org-glance-headline:contents headline))
    (buffer-hash)))

(cl-defun org-glance-headline:plain-title (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (with-temp-buffer
    (insert (org-glance-headline:title headline))
    (org-glance--remove-links 'org-glance-overview 'org-glance-state)
    (org-glance--substitute-links)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; TODO replace all implicit ...:at-point methods with the explicit pure functions
(cl-defun org-glance-headline:add-log-note (string &rest objects)
  (org-glance-headline:with-headline-at-point
   (goto-char (org-log-beginning t))
   (insert (apply #'format string objects) "\n")))

(cl-defun org-glance-headline:equal? (a b)
  (cl-check-type a org-glance-headline)
  (cl-check-type b org-glance-headline)
  (and (string= (org-glance-headline:hash a) (org-glance-headline:hash b))))

(cl-defun org-glance-headline:timestamps (headline)
  (cl-check-type headline org-glance-headline)
  (with-temp-buffer
    (insert (org-glance--decode-string (org-element-property :contents headline)))
    (cl-loop for timestamp in (-some->> (org-glance-datetime-headline-timestamps)
                                (org-glance-datetime-filter-active)
                                (org-glance-datetime-sort-timestamps))
             collect (org-element-property :raw-value timestamp))))

(cl-defun org-glance-headline:clocks (headline)
  (cl-check-type headline org-glance-headline)
  (with-temp-buffer
    (insert (org-glance--decode-string (org-element-property :contents headline)))
    (cl-loop while (re-search-forward org-clock-line-re (point-max) t)
             collect (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(cl-defun org-glance-headline:tag-string (headline)
  (cl-check-type headline org-glance-headline)
  (concat ":" (s-join ":" (mapcar #'org-glance-tag:to-string (org-glance-headline:tags headline))) ":"))

(cl-defun org-glance-headline:overview (headline)
  "Return HEADLINE high-level usability characteristics."
  (cl-check-type headline org-glance-headline)
  (with-temp-buffer
    (insert (org-glance--decode-string (org-element-property :contents headline)))
    (cl-flet ((org-list (&rest items) (org-glance--join-leading-separator-but-null "\n- " items))
              (org-newline (&rest items) (org-glance--join-leading-separator-but-null "\n" items)))
      (let* ((timestamps (org-glance-headline:timestamps headline))
             (clocks (org-glance-headline:clocks headline))
             ;; (relations (org-glance-headline-relations))
             (tags (org-glance-headline:tag-string headline))
             (state (org-glance-headline:state headline))
             (id (org-glance-headline:id headline))
             (title (org-glance-headline:plain-title headline))
             (priority (org-glance-headline:priority headline))
             (closed (org-element-property :closed headline))
             (schedule (org-glance-headline:schedule headline))
             (deadline (org-glance-headline:deadline headline))
             (encrypted (org-glance-headline:encrypted? headline))
             (linked (org-glance-headline:linked? headline)))
        (with-temp-buffer (insert
                           (concat
                            "* "
                            state
                            (if (string-empty-p state) "" " ")
                            (if priority (concat "[#" (char-to-string priority) "]" " ") "")
                            title
                            (if (string-empty-p tags) "" " ")
                            tags
                            "\n"

                            (if (and closed (listp closed))
                                (concat "CLOSED: "
                                        (org-element-property :raw-value closed)
                                        (if (or schedule deadline)
                                            " "
                                          ""))
                              "")

                            (if schedule
                                (concat "SCHEDULED: "
                                        (org-element-property :raw-value schedule)
                                        (if deadline
                                            " "
                                          ""))
                              "")

                            (if deadline
                                (concat "DEADLINE: " (org-element-property :raw-value deadline))
                              "")

                            (if (or schedule deadline closed)
                                "\n"
                              "")

                            ":PROPERTIES:\n"
                            ":ORG_GLANCE_ID: " id "\n"
                            ":DIR: " (abbreviate-file-name default-directory) "\n"
                            ":END:"

                            (org-glance--join-leading-separator-but-null "\n\n"
                              (list

                               (when (or encrypted linked)
                                 (concat "*Features*"
                                         (org-list
                                          (when encrypted "Encrypted")
                                          (when linked "Linked"))))

                               (when timestamps
                                 (concat "*Timestamps*" (apply #'org-list timestamps)))

                               ;; (when relations
                               ;;   (concat "*Relations*" (apply #'org-list (mapcar #'org-glance-relation-interpreter relations))))

                               (when clocks
                                 (concat "*Time spent*" (apply #'org-newline clocks)))))))
          (condition-case nil
              (org-update-checkbox-count-maybe 'all)
            (error nil)))))
    (s-trim (buffer-string))))

(cl-defun org-glance-headline:buffer-headlines (buffer)
  "Extract headlines from BUFFER."
  (with-current-buffer buffer
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (org-glance-headline? headline)
          (save-excursion
            (goto-char (org-glance-headline:begin headline))
            (org-glance-headline:update (org-glance-headline:from-element (org-element-at-point))
              :buffer buffer
              :file (buffer-file-name buffer))))))))

(cl-defun org-glance-headline:select-by-title (title headlines)
  (--first (string= (org-glance-headline:plain-title it) title) headlines))

(cl-defun org-glance-headline:encrypt (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance-headline:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance--encrypt-region beg end password))))

(cl-defun org-glance-headline:decrypt (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (org-glance-headline:with-headline-at-point
   (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t) (point))))
     (org-glance--decrypt-region beg end password))))

(cl-defun org-glance-headline:demote (level)
  (cl-loop repeat level
           do (org-with-limited-levels
               (org-map-tree 'org-demote))))

(cl-defun org-glance-headline:visit (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))

  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file-name headline))
         (buffer (org-glance-headline:buffer headline))
         (revert-without-query (list file)))

    (cond ((and buffer (buffer-live-p buffer)) (switch-to-buffer buffer))
          ((and file (file-exists-p file)) (find-file file))
          (t (error "Unable to visit headline: location not found.")))

    (widen)

    (org-glance-headline:search-buffer-by-id id)))

(provide 'org-glance-headline)
