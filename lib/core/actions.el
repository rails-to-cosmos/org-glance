(require 'org-glance-module)
(require 'org-element)
(require 'transient)

(org-glance-module-import lib.core.view)

(defun org-glance-action--transient-args nil
  (transient-args 'org-glance-form-action))

(cl-defmethod org-glance-generic-method-name ((name symbol))
  (intern (format "org-glance-action-%s" name)))

(cl-defmethod org-glance-concrete-method-name ((name symbol) (type symbol))
  (org-glance-concrete-method-name name (list type)))

(cl-defmethod org-glance-concrete-method-name ((name symbol) (type list))
  (->> type
    (-map #'symbol-name)
    (-sort #'s-less?)
    (s-join "-")
    (format "org-glance-action-%s-%s" name)
    (intern)))

(cl-defun org-glance-action-call (name &key (on 'current-headline) (for 'all))
  (let ((headline (cond ((eq on 'current-headline) (org-element-at-point))
                        (t on)))
        (fn (intern (format "org-glance--%s--%s" name for))))
    (unless (fboundp fn)
      (user-error "Unbound function %s" fn))
    (funcall fn headline)))

(defun org-glance-action-headlines (action)
  (cl-loop for view being the hash-values of org-glance-views
     when (org-glance-view-action-resolve view action)
     append (mapcar #'(lambda (headline) (cons headline view)) (org-glance-view-headlines/formatted view))))

(cl-defgeneric org-glance-action-register (name type)
  "Register action NAME and limit access to it for view of specific TYPE.")

(cl-defmethod org-glance-action-register ((name symbol) (type symbol))
  "Register action NAME for views of specific TYPE.
Type can be symbol that specifies 'all views or specific view."
  (org-glance-action-register name (list type)))

(cl-defmethod org-glance-action-register ((name symbol) (type list))
  "Register action NAME for views of specific TYPEs.
Type can be list of symbols to specify views."
  (let ((type (cl-pushnew type (gethash name org-glance-view-actions) :test #'seq-set-equal-p)))
    (puthash name type org-glance-view-actions)))

(defmacro org-glance-action-define (name args _ type &rest body)
  "Defun method NAME (ARGS) BODY.
Make it accessible for views of TYPE in `org-glance-view-actions'."
  (declare (debug
            ;; Same as defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
                     symbolp
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 6)
           (indent 4))

  (org-glance-action-register name type)

  (let* ((res (cl--transform-lambda (cons args body) name))
         (generic-fn (org-glance-generic-method-name name))
         (concrete-fn (org-glance-concrete-method-name name type))
         (action-private-method (intern (format "org-glance--%s--%s" name type)))
	 (form `(progn
                  (unless (fboundp (quote ,generic-fn))
                    (defun ,generic-fn (&optional args)
                      (interactive (list (org-glance-action--transient-args)))
                      (let* ((action (quote ,name))
                             (headlines (org-glance-action-headlines action))
                             (choice (unwind-protect
                                          (org-completing-read (format "%s: " action) headlines)
                                       nil))
                             (view (progn
                                     ;; (message "Choice: %s" choice)
                                     ;; (message "Headlines: %s" headlines)
                                     (alist-get choice headlines nil nil #'string=)))
                             (method-name (->> action
                                            (org-glance-view-action-resolve view)
                                            (org-glance-concrete-method-name action)))
                             (headline (replace-regexp-in-string "^\\[.*\\] " "" choice)))
                        (funcall method-name args view headline))))

                  (defun ,concrete-fn (&optional args view headline)
                    (interactive (list (org-glance-action--transient-args)))
                    (let ((org-glance-prompt (org-glance-view-prompt view (quote ,name))))
                      (org-glance
                       :default-choice headline
                       :scope (or (org-glance-view-scope view) org-glance-default-scope)
                       :db (org-glance-view-metadata-location view)
                       :filter (org-glance-view-filter view)
                       :action (function ,action-private-method))))

                  (defun ,action-private-method
                      ,@(cdr res)))))

    (if (car res)
        `(progn ,(car res) ,form)
      form)))

(org-glance-module-provide)
