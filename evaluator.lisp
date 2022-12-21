;;;; evaluator.lisp

(in-package #:teapot)

(defstruct EVariable
  (label nil :type symbol))

(defstruct ELiteral
  (value nil :type (or number string boolean)))

(defstruct ESymbol
  (label nil :type (or string list)))

(defstruct EList
  (content nil :type list))

(defstruct ENative
  (operation nil :type function))

(defstruct Expression
  (value nil :type (or symbol string boolean list))
  (struct nil :type (or symbol string boolean list))
  (position nil :type list))

(defparameter *environment* (make-hash-table))

(defmethod eval-with-environment (env (expr ELiteral))
  (declare (ignorable env))
  expr)

(defmethod eval-with-environment (env (expr EVariable))
  (declare (ignorable env))
  (let ((result (gethash (evariable-label expr) env)))
    (if result
        result
        (error "Unbound variable"))))

(defmethod eval-with-environment (env (expr EList))
  (let* ((content (mapcar (lambda (x) (second (al:flatten x)))
                          (elist-content expr)))
         (first-symbol-list (car content)))
    (if (evariable-p first-symbol-list)
        (let ((symbol-in-env (gethash (evariable-label first-symbol-list) env)))
          ;; (print (evariable-label first-symbol-list))
          (cond
            ((enative-p symbol-in-env) (funcall (enative-operation symbol-in-env)
                                                (cdr (mapcar (lambda (x) (second (al:flatten x)))
                                                             (elist-content expr)))))
            (t (error "Invalid symbol invocation.")))))))

(defun eval-teapot (expr)
  (eval-with-environment *environment* expr))

(defun prelude ()
  (setf (gethash '|print| *environment*)
        (teapot-print))
  (setf (gethash '|+| *environment*)
        (teapot-arithmetic #'+ 0))
  (setf (gethash '|-| *environment*)
        (teapot-arithmetic #'- 0)))

(defun teapot-print ()
  (labels ((validate-print (content)
             (if (= (length content) 1)
                 (print content)
                 (error "This function accepts only one parameter."))))
    (make-enative :operation #'validate-print)))

(defun teapot-arithmetic (op init-val)
  (labels ((calc-arith (content)
             (make-eliteral
              :value (reduce
                      (lambda (acc elem)
                        (funcall op acc elem))
                      content
                      :initial-value init-val
                      :key #'eliteral-value))))
    (make-enative :operation #'calc-arith)))

(defun interpret ()
  (let ((file (car (uiop:command-line-arguments))))
    (prelude)
    (eval-teapot (cadar (ep:parse 'sexp (uiop:read-file-string file))))))
