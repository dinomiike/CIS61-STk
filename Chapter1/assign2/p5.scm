;; Exercise 1.40
;; Using all code from the book needed to run "newtons-method".
;; My "cubic" procedure is at the very bottom.
;; I am testing with x^3 + ax^2 + bx +c (or ax^3 + bx^2 + cx + d) where...
;; a = 1, b = 0.1, c = -2.2, d = 5.9
;; The numbers I got when graphing a function where the curve crosses x-axis close to a whole number

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube n)
  (* n n n))

(define (square m)
  (* m m))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
