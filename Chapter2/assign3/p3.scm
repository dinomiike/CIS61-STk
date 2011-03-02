(define (next-perf n)
  (define (get-factor start)
    (if (< start n) (if (= (remainder n start) 0) start
			0)
	0))
  (define (inc x)
    (+ x 1))
  (define (sum term a next b)
    (if (> a b)
	0
	(+ (term a)
	   (sum term (next a) next b))))
  (if (= (sum get-factor 1 inc n) n) n
      (next-perf (+ n 1))))

