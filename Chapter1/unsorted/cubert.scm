(define (cubert x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (cubert-iter guess)
    (if (good-enough? guess)
        guess
        (cubert-iter (improve guess))))
  (cubert-iter 1.0)
)

(define (cube n)
	(* n n n)
)

(define (average p q)
	(/ (+ p q) 2)
)
