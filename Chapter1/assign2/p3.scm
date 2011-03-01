;; Exercise 1.29
;; Where change of x (h) is h = (b - a)/n
;; y = f(a + kh)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc arg)
  (+ arg 1))

(define (cube x)
  (* x x x))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((even? k) 2)
	     ((odd? k) 4))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

;; =================================================================
;; The integral method provided in the book, used for comparison

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
