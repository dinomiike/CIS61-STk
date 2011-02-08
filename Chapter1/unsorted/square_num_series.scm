(define (sq_num_series x)
	(define loop 1)
	;; Assumes you always have one parameter
	(define y (sentence (square (item 1 x))))
	(define (square n)
		(* n n)
	)
	(define (proc)
		(if (<= (count x) loop) 
			(
				;; Then
				(define loop (+ loop 1)) 
				(define y (sentence y (square (item loop x)))) 
				proc
			)
			(
				;; Else
				y
			)
	)
	(sentence (proc))
)

(define (sq_num_series x)
	(define loop 1)
	;; Assumes you always have one parameter
	(define y (sentence (square (item 1 x))))
	(define (square n)
		(* n n)
	)
	(define (proc)
		(if (<= loop (count x))
			(
				(define loop (+ loop 1)) 
				(define y (sentence y (square (item loop x))))
				(proc)
			)
			y
		)
	)
	(sentence (proc))
)



(cond ((= 1 1)
	(define x 9)
	(define y 10))
	(else 
	(define x 1)
	(define y 1))
)


(define (square n)
	(* n n))

(define loop 0)

(define y 'holding)

(define (squares x)
	(cond ((< loop (count x))
		(define loop (+ loop 1))
		(define y (sentence y (square (item loop x))))
		(squares x))
		(else y)
	)
)


(define (squares x)
       (cond ((> (count x) 0)
              (define y (sentence y (square (first x))))
              (squares (bf x)))
             (else y))
)

(define (squares x)
	(if (= (count x) 1)
		(se (squares x))
		(se (squares (first x)) (squares (bf x)))
	)
)
