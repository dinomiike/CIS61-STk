;; Problem 1

(define (cubert x)
        (define (good-enough? last-guess guess)
                (<
                        (if (< last-guess guess) (/ guess last-guess)
                                (/ last-guess guess)
                        )
                1.001)
        )
        (define (improve guess)
                (/ (+ (/ x (square guess)) (* 2 guess)) 3)
        )
        (define (cubert-iter last-guess guess)
                (if (good-enough? last-guess guess) guess
                        (cubert-iter guess (improve guess))
                )
        )
        (define (square n)
                (* n n)
        )
        (cubert-iter 2.0 1.0)
)
