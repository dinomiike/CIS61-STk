(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define rev-us-coins (list 1 5 10 25 50))
(define rev-uk-coins (list 0.5 1 2 5 10 20 50 100))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;==================================================================

(define (no-more? coin-values)
  (if (null? coin-values) #t
      #f))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

;; It doesn't matter if the list is in order. It will recursively goes through
;; every sublist after getting the value of the first coin from the initial amount entered
