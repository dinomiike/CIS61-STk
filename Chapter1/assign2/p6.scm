;; CIS61 Assignment 2
;; Problem 6 (Exercise 1.41)
;; double is a procedure that accepts a procedure as an argument and runs that argument twice on
;; whatever parameter it gets. You need lambda to hold the parameter passed to the compound procedure
;; as double itself just returns a procedure, not a value.
;; The answer to the question in the book is 21, as seen in the attached .out file.

(define (double f)
  (lambda (x) (f (f x))))

(define (inc arg)
  (+ arg 1))
