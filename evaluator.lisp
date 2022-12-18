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
  (gethash (evariable-label expr) env))

(defmethod eval-with-environment (env (expr EList))
  (let ((first-symbol-list (caaar (elist-content expr))))
    (cond
      ((eql '|print| first-symbol-list) (print (caadr (elist-content expr)))))))

(defun eval-teapot (expr)
  (eval-with-environment *environment* expr))

#||

(defmethod eval-with-environment (env (expr LiteralT))
  (declare (ignorable env))
  expr)

(defmethod eval-with-environment (env (expr VariableT))
  (multiple-value-bind (value ok) (gethash (variablet-label expr) env)
    (if ok
	value
	(error 'simple-error :format-arguments (list (variablet-label expr))
	                     :format-control "Unbound variable ~s."))))

(defmethod eval-with-environment (env (expr AbstractionT))
  (make-closuret :var (abstractiont-param expr)
		 :expression (abstractiont-body expr)
		 :environment (al:copy-hash-table env)))

(defmethod eval-with-environment (env (expr ApplicationT))
  (let ((arg (eval-with-environment env (applicationt-argument expr)))
	(f   (eval-with-environment env (applicationt-abstraction expr))))
    (etypecase f
      (ClosureT
       (let ((closedEnv (sp:merge-tables env (closuret-environment f))))
	 (setf (gethash (closuret-var f) closedEnv) arg)
	 (eval-with-environment closedEnv (closuret-expression f)))))))

(defmethod eval-with-environment (env (expr ClosureT))
  (declare (ignorable env))
  expr)

(eval-with-environment *environment*
		       (make-applicationt :abstraction (make-abstractiont
							:param 'x
							:body (make-variablet :label 'x))
					  :arguments (list (make-literalt :value 123))))

||#
