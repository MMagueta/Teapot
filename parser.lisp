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


;; (pchar '(' .>> spaces) >>. many pexpression .>> (spaces >>. pchar ')')

;;; A semantic predicate for filtering out double quotes.

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;; Utility rules.

(ep:defrule whitespace (ep:+ (or #\space #\tab #\newline))
  (:constant nil))

(ep:defrule alphanumeric (or (alphanumericp character)))

(ep:defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

(ep:defrule sexp (and (ep:? whitespace)
		      (or magic list atom))
  (:function second)
  (:lambda (s ep:&bounds start end)
    (list s (cons start end))))

(ep:defrule magic "foobar"
  (:constant :magic)
  (:when (eq ep:* :use-magic)))

(ep:defrule list (and #\( sexp (ep:* sexp) (ep:? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (cons car cdr)))

(ep:defrule atom (or string integer symbol quotation))

(ep:defrule string (and #\" (ep:* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (make-literalt :value (ep:text string))))

(ep:defrule quotation (and #\' list)
  (:destructure (quotation elem)
    (declare (ignore quotation))
    (make-literalt :value (ep:text elem))))

(ep:defrule integer (ep:+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (make-literalt :value (parse-integer (ep:text list) :radix 10))))

(ep:defrule symbol (not-integer (ep:+ alphanumeric))
  ;; NOT-INTEGER is not strictly needed because ATOM considers INTEGER before
  ;; a STRING, we know can accept all sequences of alphanumerics -- we already
  ;; know it isn't an integer.
  (:lambda (list)
    (make-variablet :label (intern (ep:text list)))))

;;;; Try these

(ep:parse 'sexp "(FOO123)")

(ep:parse 'sexp "123")

(ep:parse 'sexp "\"foo\"")

(ep:parse 'sexp "  (  1 2  3 (FOO\"foo\"123 )   )")

(ep:parse 'sexp "foobar")

(let ((* :use-magic))
  (ep:parse 'sexp "foobar"))

(describe-grammar 'sexp)

(trace-rule 'sexp :recursive t)

(ep:parse 'sexp "(foo bar 1 quux)")

(untrace-rule 'sexp :recursive t)

(defparameter *orig* (rule-expression (find-rule 'sexp)))

(change-rule 'sexp '(and (? whitespace) (or list symbol)))

(ep:parse 'sexp "(foo bar quux)")

(ep:parse 'sexp "(foo bar 1 quux)" :junk-allowed t)

(change-rule 'sexp *orig*)

(ep:parse 'sexp "(foo bar 1 quux)" :junk-allowed t)
