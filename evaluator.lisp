;;;; evaluator.lisp

(in-package #:teapot)

;(defstruct EVariable
;  (label nil :type symbol))

(defstruct ELiteral
  (value nil :type (or number string boolean)))

(defstruct ESymbol
  (label nil :type (or string list)))

(defstruct EList
  (content nil :type list))

(defstruct ENative
  (operation nil :type (or null function)))

(defstruct EFunction
  (args nil :type list)
  (body nil :type list)
  (local-env nil :type list))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defparameter *environment* (make-hash-table)))

(defmethod eval-with-environment (env (expr ELiteral))
  (declare (ignorable env))
  expr)

#||
(defmethod eval-with-environment (env (expr EVariable))
  (declare (ignorable env))
  (let ((result (gethash (evariable-label expr) env)))
    (or result
        (error "Unbound variable"))))
||#

(defmethod eval-with-environment (env (expr ESymbol))
  (let ((result (gethash (evariable-label expr) env)))
    (or result
        (error "Unbound variable"))))

(defmethod eval-with-environment (env (expr EList))
  (let* ((content (elist-content expr))
         (first (first content)))
    (let ((evaluated (handler-case (eval-with-environment env first)
		       (error () first))))
      (etypecase evaluated
        (eliteral (error "Cannot funcall a literal"))
	(esymbol (error "Cannot funcall a symbol"))
	(efunction (assert (= (length (rest content))
			      (length (efunction-args content))))
	 (let* ((evaluated-args (mapcar (lambda (x) (eval-with-environment env (posdata-data x)))
					(rest content)))
		(new-env (sp:merge-tables env (efunction-local-env evaluated))))))
        (enative (funcall (efunction-operation evaluated)
                          (mapcar (lambda (x) (eval-with-environment env (posdata-data x)))
				  (rest content))))))))

(defun eval-teapot (expr)
  (eval-with-environment *environment* expr))

(defun prelude ()
  (setf (gethash 'teapot::|print| *environment*)
        (teapot-print))
  (setf (gethash 'teapot::|+| *environment*)
        (teapot-arithmetic #'+ 0))
  (setf (gethash 'teapot::|-| *environment*)
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
