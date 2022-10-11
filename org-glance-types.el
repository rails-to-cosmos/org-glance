;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)

(cl-defmacro org-glance:declare-type (f key-types &rest types)
  "Attach the given list of types to the function ‘f’
   by advising the function to check its arguments’ types
   are equal to the list of given types.

   We name the advice ‘⟪f⟫-typing-advice’ so that further
   invocations to this macro overwrite the same advice function
   rather than introducing additional, unintended, constraints.

   Using type specifiers we accommodate for unions of types
   and subtypes, etc ♥‿♥.

   ‘key-types’ should be of the shape (:x₀ t₀ ⋯ :xₙ tₙ);
    when there are no optional types, use symbol “:”.

    E.g., (declare-type my-func (:z string :w integer) integer symbol string)
  "

  ;; Basic coherency checks. When there aren't optional types, key-types is the “:” symbol.
  (cl-assert (and (listp types) (or (listp key-types) (symbolp key-types))))

  (cl-letf* ((pairify (lambda (xs) (cl-loop for i in xs by #'cddr         ;; Turn a list of flattenned pairs
                                 for j in (cdr xs) by #'cddr   ;; into a list of explicit pairs.
                                 collect (cons i j))))         ;; MA: No Lisp method for this!?
             (result-type  (car (-take-last 1 types)))
             (types        (-drop-last 1 types))
             (num-of-types (length types))
             (key-types-og (unless (symbolp key-types) key-types))
             (key-types    (funcall pairify key-types-og))
             (advice-name  (intern (format "%s-typing-advice" f)))
             (notify-user  (format "%s now typed %s → %s → %s."
                                   `,f key-types-og types result-type)))

    `(progn
       (cl-defun ,advice-name (orig-fun &rest args)

         ;; Split into positional and key args; optionals not yet considered.
         (cl-letf* ((all-args
                     (-split-at
                      (or (--find-index (not (s-blank? (s-shared-start ":" (format "%s" it)))) args) ,num-of-types)
                      args)) ;; The “or” is for when there are no keywords provided.
                    (pos-args  (car all-args))
                    (key-args  (funcall ,pairify (cadr all-args)))
                    (fun-result nil)
                    ((symbol-function 'shucks)
                     (lambda (eτ e g)
                       (unless (cl-typep g eτ)
                         (error "%s: Type mismatch! Expected %s %s ≠ Given %s %s."
                                (function ,f) eτ e (type-of g) (prin1-to-string g))))))

           ;; Check the types of positional arguments.
           (unless (equal ,num-of-types (length pos-args))
             (error "%s: Insufficient number of arguments; given %s, %s, but %s are needed."
                    (function ,f) (length pos-args) pos-args ,num-of-types))
           (cl-loop for (ar ty pos) in (-zip pos-args (quote ,types) (number-sequence 0 ,num-of-types))
              do (shucks ty (format "for argument %s" pos) ar))

           ;; Check the types of *present* keys.
           (cl-loop for (k . v) in key-args
              do (shucks (cdr (assoc k (quote ,key-types))) k v))

           ;; Actually execute the orginal function on the provided arguments.
           (setq fun-result (apply orig-fun args))
           (shucks (quote ,result-type) "for the result type (!)" fun-result)

           ;; Return-value should be given to caller.
           fun-result))

       ;; Register the typing advice and notify user of what was added.
       (advice-add (function ,f) :around (function ,advice-name))
       ,notify-user)))

(cl-deftype org-glance-file ()
  '(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-deftype org-glance-directory ()
  '(satisfies (lambda (location) (and (f-dir-p location)
                                 (f-readable-p location)))))

(cl-defun org-glance:list-of-p (tp thing)
  (and (listp thing) (cl-every (lambda (x) (cl-typep x tp)) thing)))

(cl-deftype org-glance:list-of (tp)
  `(satisfies (lambda (thing) (org-glance:list-of-p (quote ,tp) thing))))

(provide 'org-glance-types)
