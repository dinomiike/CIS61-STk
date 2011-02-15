;; Aaron (Miike) Ramos
;; CIS61 Assignment 2 - Problem 2
;; 2/12/11
;; Function returns the number of a given letter in a word or sentence

(define (p2 test item)
  (define (ugly_function item)
    (appearances test item))
  (accumulate + (every ugly_function item)))
