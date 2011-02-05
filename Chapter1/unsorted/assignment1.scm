;;(define (sqrt x)
;;  (define (good-enough? guess)
;;    (< (abs (- (square guess) x)) 0.001))
;;  (define (improve guess)
;;    (average guess (/ x guess)))
;;  (define (sqrt-iter guess)
;;    (if (good-enough? guess)
;;        guess
;;        (sqrt-iter (improve guess))))
;;  (sqrt-iter 1.0)
;;)

;;(define (square n)
;;	(* n n)
;;)

;;(define (average p q)
;;	(/ (+ p q) 2)
;;)

;;==========================================================================

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

;;===========================================================================

;; Problem 3

(define (query x)
	(sentence (first (bf x)) (first x) (bf (bf x)) '(?))
)

;;===========================================================================

STk> (last 1)
1
STk> (last 12)
2
STk> series
(1 2 3 4 5)
STk> (count 1)
1
STk> (count 14)
2
STk> (count '(test))
1
STk> (count '(this is a test))
4
STk> (count series)
5
STk> (item 1 series)
1
STk> (item 2 series)
2
STk> (item 5 series)
5
STk> (last series)
5
STk> (define series (series '(6)))
*** Error:
    eval: bad function in : (series (quote (6)))
Current eval stack:
__________________
  0    (series (quote (6)))
  1    (define series (series (quote (6))))
STk> (define series (series 6))
*** Error:
    eval: bad function in : (series 6)
Current eval stack:
__________________
  0    (series 6)
  1    (define series (series 6))
STk> (define z 1)
z
STk> z
1
STk> (define z (+ z 1))
z
STk> z
2
STk> series
(1 2 3 4 5)
STk> (sentence series (series '6))
*** Error:
    eval: bad function in : (series (quote 6))
Current eval stack:
__________________
  0    (series (quote 6))
  1    (sentence series (series (quote 6)))
STk> (sentence series (series '(6)))
*** Error:
    eval: bad function in : (series (quote (6)))
Current eval stack:
__________________
  0    (series (quote (6)))
  1    (sentence series (series (quote (6))))
STk> (define series (series '6))
*** Error:
    eval: bad function in : (series (quote 6))
Current eval stack:
__________________
  0    (series (quote 6))
  1    (define series (series (quote 6)))
STk> (define series (series 6))
*** Error:
    eval: bad function in : (series 6)
Current eval stack:
__________________
  0    (series 6)
  1    (define series (series 6))
STk> (define series (series '(6)))
*** Error:
    eval: bad function in : (series (quote (6)))
Current eval stack:
__________________
  0    (series (quote (6)))
  1    (define series (series (quote (6))))
STk> series
(1 2 3 4 5)
