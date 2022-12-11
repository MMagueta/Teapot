;;;; teapot.lisp

(in-package #:teapot)

(deftype ExpressionT ()
  `(or AbstractionT VariableT LiteralT ApplicationT ArithmeticT null))

(defstruct ArithmeticT
  (operation nil :type symbol)
  (parameters nil :type list))

(defstruct AbstractionT
  (param nil :type symbol)
  (body nil :type ExpressionT))

(defstruct VariableT
  (label nil :type symbol))

(defstruct LiteralT
  (value nil :type (or number string boolean)))

(defstruct ClosureT
  (var nil :type symbol)
  (expression nil :type ExpressionT)
  (environment (make-hash-table) :type hash-table))

(defstruct ApplicationT
  (abstraction nil :type ExpressionT)
  (argument nil :type ExpressionT))

(defstruct ConditionT
  (condition nil :type ExpressionT)
  (then nil :type ExpressionT)
  (else nil :type ExpressionT))

(defstruct NativeT
  (fun nil :type ExpressionT))

(defparameter *environment* (make-hash-table))

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
					  :argument (make-literalt :value 123)))
