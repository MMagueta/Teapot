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

(defstruct posdata
  (data nil)
  (start 0 :type (integer 0))
  (end 0 :type (integer 0)))

(ep:defrule whitespace (ep:+ (or #\space #\tab #\newline))
  (:constant nil))

(ep:defrule alphanumeric (or (alphanumericp character)))

(ep:defrule string-char (or (not-doublequote character) (and #\\ #\")))

(ep:defrule sexp (and (ep:? whitespace)
                      (or list atom)
                      (ep:? whitespace))
  (:function second)
  (:lambda (s ep:&bounds start end)
    (make-posdata :data s :start start :end end)))

(ep:defrule list (and #\( (ep:? sexp) (ep:* sexp) (ep:? whitespace) #\))
  (:destructure (p1 car cdr w p2)
    (declare (ignore p1 p2 w))
    (if (or car cdr)
        (make-elist :content (cons car cdr))
        (make-elist :content nil))))




(ep:defrule atom (or quotation string integer symbol))

(ep:defrule string (and #\" (ep:* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (make-eliteral :value (ep:text string))))

(ep:defrule quotation (and #\' sexp)
  (:destructure (quotation elem)
    (declare (ignore quotation))
    (let ((data (posdata-data elem)))
      (etypecase data
        (eliteral elem)
        (esymbol (make-posdata :data (make-elist :content
                                                 (list (make-esymbol :label '|quote|)
                                                       data))
                               :start (- (posdata-start elem) 1)
                               :end (posdata-end elem)))
        (elist (make-posdata :data (make-elist :content
                                               (cons (make-esymbol :label '|quote|)
                                                     (elist-content data)))
                             :start (- (posdata-start elem) 1)
                             :end (posdata-end elem)))))))


(ep:defrule integer (ep:+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (make-eliteral :value (parse-integer (ep:text list) :radix 10))))

(ep:defrule symbol (or (not-integer (ep:+ alphanumeric)) #\+ #\-)
  (:lambda (list)
    (make-esymbol :label (intern (ep:text list) "TEAPOT"))))
