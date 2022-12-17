(in-package #:teapot)

(defun thunk-consume-tokens (list)
  (lambda ()
    (let ((value (pop list)))
      (if (null value)
	  (values nil nil)
	  (values (lex:token-class value) value)))))

(yacc:define-parser lisp-parser
  (:start-symbol form)
  (:terminals (|(| |)| |.| identifier integer))
  (s-expression
   identifier
   integer
   (list #'al:flatten)
   (|(| s-expression |.| s-expression |)|))
  
  (list
   (|(| s-expression form |)|))

  (form 
   s-expression
   (s-expression form)))

(lexer:define-lexer lisp-lexer (state)
  ("[a-zA-Z0-9\\-]+" (values 'identifier $$))
  ("[0-9]+" (values 'integer (parse-integer $$)))
  ("%(" (values '|(| $1))
  ("%)" (values '|)| $1))
  ("%'" (values 'quote $1))
  ("quote" (values 'quote $1))
  ("%." (values '|.| $1))
  ("%s" (values :next-token))
  ("%n" (values :next-token)))

;; (make-applicationt :abstraction (make-abstractiont :param "x"))

;; [(; abc; 1; 2; 3; )] => application [variable(abc)] 1 2 3

(defmethod match-token ((kind (eql 'identifier))
			lexeme)
  (make-variablet :label (make-symbol lexeme)))

;; (defmethod match-token ((kind (eql '))
;; 			lexeme)
;;   (make-variablet :label (make-symbol lexeme)))

(defun map-ast (tokens)
  (let ((kind (lex:token-class (car tokens)))
	(lexeme (lex:token-lexeme (car tokens))))
    (match-token kind lexeme)))

(defun lispify2 (tokens)
  (let ((stack (list (list))))
    (loop :for tok :in tokens
          :do (case (lex:token-class tok)
                (|(| (push (list) stack))
                (|)| (push (nreverse (pop stack)) (first stack)))
                (otherwise (push tok (first stack)))))
    (first stack)))

(defparameter *expr* (lispify2
		      (yacc:parse-with-lexer (thunk-consume-tokens
					      (lexer:tokenize 'lisp-lexer "(abc 1 (foo 2) (bar 3))")) lisp-parser)))

(defmethod convert-ast ((elem (eql 'if)))
  '(make-conditiont ))

(defmethod convert-ast ((elem (eql 'if)) &rest args)
  '(make-conditiont ))

(defmethod convert-ast ((elem symbol) &rest args)
  (make-applicationt :abstraction elem :arguments args))

(defmethod covert-ast ((parser-input list) &rest args)
  (declare (ignore args))
  (make-applicationt :abstraction (first parser-input) :arguments (mapcar #'convert-ast (rest parser-input))))
