;; Assignment 7
;; Problem 2

(define (fract-stream x)
  (if (null? x) (error "Null list")
      (let ((numer (car x))
	    (denom (cadr x)))
	(cond ((> numer denom) (error "Numerator greater than denominator"))
	      ((or (< numer 0) (< denom 0)) (error "Must use positive integers"))
	      ((divisible? numer denom) (cons-stream (/ numer denom) the-empty-stream))
	      (else (cons-stream (truncate (/ (* numer 10) denom))
				 (fract-stream (list (remainder (* numer 10) denom) denom))))))))

(define (approximation fs numdigits)
  (if (= numdigits 0) nil
      (if (stream-null? fs) (begin
			      (append (list 0)
				      (approximation fs (- numdigits 1))))
	  (append (list (stream-car fs))
		  (approximation (stream-cdr fs) (- numdigits 1))))))

(define (divisible? n1 n2)
  (= (remainder n1 n2) 0))
