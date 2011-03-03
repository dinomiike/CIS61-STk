(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

;;===============================================================

(define (best-total hand)
  (define (strip card)
    (if (> (count card) 2) 10
	(if (equal? (first card) 'a) 0
	    (first card))))
  (define (new-strip card)
    (cond ((> (count card) 2) 10)
	  ((equal? (first card) 'a) 0)
	  ((or (equal? (first card) 'k) (equal? (first card) 'q) (equal? (first card) 'j)) 10)
	  (else (first card))))
  (define (is-ace? card)
    (if (equal? (first card) 'a) #t #f))
  (define (widdle value aces)
    (cond ((= aces 0) value)
	  (else (if (<= (+ value 11) 21) (widdle (+ value 11) (- aces 1))
		    (widdle (+ value 1) (- aces 1))))))
  (define (eval-aces value aces)
    (cond ((= aces 4) (if (> (+ value 14) 21) (+ value 4)
			  (+ value 14)))
	  ((= aces 3) (if (> (+ value 13) 21) (+ value 3)
			  (+ value 13)))
	  ((= aces 2) (if (> (+ value 12) 21) (+ value 2)
			  (+ value 12)))
	  ((= aces 1) (if (> (+ value 11) 21) (+ value 1)
			  (+ value 11)))))
  (let ((aces (count (keep is-ace? hand)))
	(prelim-value (accumulate + (every new-strip hand))))
    (if (> aces 0) (eval-aces prelim-value aces)
	prelim-value)))

;;=================================================================

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (if (< (best-total customer-hand-so-far) 17) #t #f))

;;=================================================================

(define (play-n strategy n)
  (play-x 0 strategy n))

(define (play-x result strategy x)
  (if (= x 0) result
      (play-x (+ result (twenty-one strategy)) strategy (- x 1))))

;;=================================================================

(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (if (or
       (and
	(or
	 (equal? dealer-up-card 'a)
	 (equal? dealer-up-card 7)
	 (equal? dealer-up-card 8)
	 (equal? dealer-up-card 9)
	 (equal? dealer-up-card 10)
	 (equal? dealer-up-card 'k)
	 (equal? dealer-up-card 'q)
	 (equal? dealer-up-card 'j))
	(< customer-hand-so-far 17))
       (and
	(or
	 (equal? dealer-up-card 2)
	 (equal? dealer-up-card 3)
	 (equal? dealer-up-card 4)
	 (equal? dealer-up-card 5)
	 (equal? dealer-up-card 6))
	(< customer-hand-so-far 12) )) #t
      #f))

;;=================================================================

(define (stop-at n)
  (lambda (customer-hand-so-far dealer-up-card) (if (< (best-total customer-hand-so-far) n) #t #f)))

;;=================================================================

(define (valentine customer-hand-so-far dealer-up-card)
  (if (member? 'h (every bf customer-hand-so-far)) (if (< (best-total customer-hand-so-far) 19) #t
						       #f)
      (if (< (best-total customer-hand-so-far) 17) #t
	  #f)))

;;=================================================================

(define (on-suit suit customer-hand-so-far dealer-up-card)
  (if (member? suit (every bf customer-hand-so-far)) (if (< (best-total customer-hand-so-far) 19) #t
							 #f)
      (if (< (best-total customer-hand-so-far) 17) #t
	  #f)))

;;=================================================================

(define (suit-strategy suit strategy1 strategy2)
  (lambda (customer-hand-so-far dealer-up-card) (if (on-suit suit customer-hand-so-far dealer-up-card) (strategy1 customer-hand-so-far dealer-up-card)
						    (strategy2 customer-hand-so-far dealer-up-card))))

;;=================================================================

(define (majority strategy1 strategy2 strategy3)
  (lambda (customer-hand-so-far dealer-up-card) (if (>= (+ (if (strategy1 customer-hand-so-far dealer-up-card) 1 0)
							   (if (strategy2 customer-hand-so-far dealer-up-card) 1 0)
							   (if (strategy3 customer-hand-so-far dealer-up-card) 1 0)) 2) #t
							   #f)))

;;=================================================================

