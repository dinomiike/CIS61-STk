;; This method receives 3 integer paramters and returns the sum of the two larger numbers squared
define (lab1_3 a b c)
         (cond ((> a b) (define x (* a a)) (cond ((> b c) (define y (* b b))) (else (define y (* c c))))) (else (define x (* b b)) (cond ((> a c) (define y (* a a))) (else (define y (* c c))))))
         (+ x y)
)
