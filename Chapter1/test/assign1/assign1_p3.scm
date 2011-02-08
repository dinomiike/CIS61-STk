;; Problem 3

(define (query x)
	(if (= (count x) 1)
		(se x '(?))
        	(se (first (bf x)) (first x) (bf (bf x)) '(?))
	)
)
