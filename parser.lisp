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
   list
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
  ("%." (values '|.| $1))
  ("%s" (values :next-token))
  ("%n" (values :next-token)))

;; (make-applicationt :abstraction (make-abstractiont :param "x"))

;; [(; abc; 1; 2; 3; )] => application [variable(abc)] 1 2 3

(defmethod match-token ((kind (eql 'identifier))
			lexeme)
  (make-variablet :label (make-symbol lexeme)))

(defmethod match-token ((kind (eql '))
			lexeme)
  (make-variablet :label (make-symbol lexeme)))

(defun map-ast (tokens)
  (let ((kind (lex:token-class (car tokens)))
	(lexeme (lex:token-lexeme (car tokens))))
    (match-token kind lexeme)))

(map-ast (al:flatten
	  (yacc:parse-with-lexer (thunk-consume-tokens (lexer:tokenize 'lisp-lexer "abc-xyz")) lisp-parser)))

;; Next step: find a way to parse lists as applications
