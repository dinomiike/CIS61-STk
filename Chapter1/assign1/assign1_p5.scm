;; Problem 5

(define (sq n)
	(* n n))

(define (squares wd)
	(if (= (count wd) 1)
		(se (sq (first wd)))
		(se (sq (first wd)) (squares (bf wd)))
	)
)
