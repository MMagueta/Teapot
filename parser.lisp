(in-package #:teapot)

#||
type Literal =
| LInteger of int
| LString of string
| LNil of bool

type Expression =
| EList of List<Expression>
| ESymbol of string
| ELiteral of Literal
| EDotList of List<Expression>*Expression
||#

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(ep:defrule whitespace (ep:+ (or #\space #\tab #\newline))
  (:constant nil))

(ep:defrule alphanumeric (or (alphanumericp character)))

(ep:defrule string-char (or (not-doublequote character) (and #\\ #\")))

(ep:defrule sexp (and (ep:? whitespace)
                      (or list atom))
  (:function second)
  (:lambda (s ep:&bounds start end)
    (list s (cons start end))))

(ep:defrule list (and #\( (ep:? sexp) (ep:* sexp) (ep:? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cond
      ((and (eql nil car)
            (eql nil cdr)) (list nil (make-elist)))
      ((not (eql nil car)) (list (cons car cdr) (make-elist :content (cons car cdr)))))))

(ep:defrule atom (or quotation string integer symbol))

(ep:defrule string (and #\" (ep:* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (make-eliteral :value (ep:text string))))

(ep:defrule quotation (and #\' sexp)
  (:destructure (quotation elem)
    (declare (ignore quotation))
    (let ((inner-symbol (caar elem)))
      (make-esymbol :label (if (listp inner-symbol)
                               inner-symbol
                               (string inner-symbol))))))

(ep:defrule integer (ep:+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (list (parse-integer (ep:text list))
          (make-eliteral :value (parse-integer (ep:text list) :radix 10)))))

(ep:defrule symbol (or (not-integer (ep:+ alphanumeric)) #\+ #\-)
  (:lambda (list)
    (list (intern (ep:text list)) (make-evariable :label (intern (ep:text list))))))
