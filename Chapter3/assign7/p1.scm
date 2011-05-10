;; Assignment 7
;; Chapter 3
;; Section 3.5 - Streams

;; Problem 3.50 ############################################################
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;; Problem 3.51 ############################################################
(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; Problem 3.52 #############################################################
;; Part 1===========================================
;; after:
;; (stream-ref y 7)
;; =>136
;; Prints the sequence of numbers from 1 to 20, stopping at 7, and filtering out the numbers which are even

;; Part 2===========================================
;;(define sum 0)
;;(define (accum x)
;;  (set! sum (+ x sum))
;;  sum)
;;(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;;(define y (stream-filter even? seq))
;;(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                         seq))
;;(stream-ref y 7)
;;(display-stream z)

;; Result:
;;10
;;15
;;45
;;55
;;105
;;120
;;190
;;210
;; Prints the result of the accumulated sums of 1 to 20 which are factors of 5.


;; Code from Section 3.5.2
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;; Problem 3.53 ##############################
;; (define s (cons-stream 1 (add-streams s s)))
;; This defines an infinite stream s whose car is 1 and whose cdr is a promise to add s to itself.
;; It should grow on each pass of the stream, adding the previous list to the current list on each pass



;; Problem 3.54 #############################
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams integers factorials)))


;; Problem 3.56 #############################
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;;(define s (cons-stream 1 (merge <??> <??>)))
(define s (cons-stream 1 (merge (scale-stream s 2)
				(merge (scale-stream s 3)
				       (scale-stream s 5)))))


;; Problem 3.64 #############################
(define (stream-limit s n)
  (let ((p1 (stream-car s))
	(p2 (car (stream-cdr s))))
    (cond ((< (abs (- p1 p2)) n) p2)
	  (else (stream-limit (stream-cdr s) n)))))

(define (sqrt2 x tolerence)
  (stream-limit (sqrt-stream x) tolerence))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
