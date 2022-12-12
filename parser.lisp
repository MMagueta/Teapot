(in-package #:teapot)

;; s_expression :=  atomic_symbol
;;                | "(" s_expression "."s_expression ")"
;;                | list 
   
;; list := "(" s_expression < s_expression > ")"

;; atomic_symbol := letter atom_part

;; atom_part := empty / letter atom_part / number atom_part
;; letter := "a" / "b" / " ..." / "z"
;; number := "1" / "2" / " ..." / "9"
;; empty := " "

(defun list-lexer (list)
  (lambda ()
    (let ((value (pop list)))
      (if (null value)
	  (values nil nil)
	  (let ((terminal
		  (cond ((member value '(+ - * / |(| |)|)) value)
			((integerp value) 'int)
			((symbolp value) 'id)
			(t (error "Unexpected value ~S" value)))))
	    (values terminal value))))))

(yacc:define-parser lisp-parser
  (:start-symbol form)
  (:terminals (\( \) symbol constant))
  (form
   atom
   list)
  
  (atom
   constant
   symbol)

  (list
   (\( symbol forms \)))

  (forms
   form
   (form forms) ))
