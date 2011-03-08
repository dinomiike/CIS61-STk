(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display")"))

(define (make-segment ns ne)
  (cons ns ne))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment ls)
  (let ((a (start-segment ls))
	(b (end-segment ls)))
    (make-point (average (x-point a) (x-point b))
		(average (y-point a) (y-point b)))))

(define (average n1 n2)
  (/ (+ n1 n2) 2))
