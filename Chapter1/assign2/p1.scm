;; Aaron Miike Ramos
;; CIS61 2/12/11
;; Assignment 2 - Problem 1
;; Note: I named my procedure "every3" so the original every wouldn't be
;; over-written by mine, and used the original function to compare the
;; results of my procedure.

(define (square n) (* n n))

(define (other fx item)
  (se (fx item)))

(define (every3 fx item)
  (se (fx (first item)) ( cond ((> (count item) 1) (every3 fx (bf item)))
			       '()
			       )
      ) )
