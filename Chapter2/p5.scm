(define (make-rat-new n d)
  (let ((g (gcd n d)))
    (if (< d 0) (cons (/ (* n -1) g) (/ (* d -1) g))
	(cons (/ n g) (/ d g)))))

(define (make-rat n d)
   (let ((g (gcd n d)))
     (if (< d 0)
         (cons (/ (* n -1) g) (/ (* d -1) g))
         (cons (/ n g) (/ d g)))))

(define (gcd a b)
  (if (= b 0)
      ;; Ensure gcd returns a positive integer
      (if (< a 0) (* a -1) a)
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x))
  (newline))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define one-half (make-rat 1 2))
