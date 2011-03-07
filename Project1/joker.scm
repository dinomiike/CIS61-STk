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

;; O = Joker
(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) 'O 'O) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )

;;===============================================================

(define (best-total hand)
  (define (new-strip card)
    (cond ((> (count card) 2) 10)
	  ((equal? (first card) 'a) 0)
	  ((equal? (first card) 'o) 0)
	  ((or (equal? (first card) 'k) (equal? (first card) 'q) (equal? (first card) 'j)) 10)
	  (else (first card))))
  (define (is-ace? card)
    (if (equal? (first card) 'a) #t #f))
  (define (eval-aces value aces jokers)
    (cond ((= aces 4) (if (> (+ value 14 jokers) 21) (+ value 4)
			  (+ value 14)))
	  ((= aces 3) (if (> (+ value 13) 21 jokers) (+ value 3)
			  (+ value 13)))
	  ((= aces 2) (if (> (+ value 12) 21 jokers) (+ value 2)
			  (+ value 12)))
	  ((= aces 1) (if (> (+ value 11) 21 jokers) (+ value 1)
			  (+ value 11)))
	  ((= aces 0) value)))
  (define (eval-jokers value jokers)
    (cond ((= jokers 2) 21)
	  ((= jokers 1) (if (<= (- 21 value) 11) (+ (- 21 value) value)
			    (+ value 11)))
	  ((= jokers 0) value)))
  (let ((aces (count (keep is-ace? hand)))
	(jokers (appearances 'O hand)))
    (eval-jokers (eval-aces (accumulate + (every new-strip hand)) aces jokers) jokers)))

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

(define (new-strip card)
  (cond ((> (count card) 2) 10)
	((equal? (first card) 'a) 0)
	((or (equal? (first card) 'k) (equal? (first card) 'q) (equal? (first card) 'j)) 10)
	(else (first card))))

;;=================================================================

(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (define (dealer-cond1? dealer-up-card)
    (if (or
	 (member? 'a dealer-up-card)
	 (member? 7 dealer-up-card)
	 (member? 8 dealer-up-card)
	 (member? 9 dealer-up-card)
	 (member? 10 (every new-strip dealer-up-card))
	 (member? 'k dealer-up-card)
	 (member? 'q dealer-up-card)
	 (member? 'j dealer-up-card)) #t
	 #f))
  (define (dealer-cond2? dealer-up-card)
    (if (or
	 (member? 2 dealer-up-card)
	 (member? 3 dealer-up-card)
	 (member? 4 dealer-up-card)
	 (member? 5 dealer-up-card)
	 (member? 6 dealer-up-card)) #t
	 #f))
  (if (or
       (and (dealer-cond1? dealer-up-card) (< (best-total customer-hand-so-far) 17))
       (and (dealer-cond2? dealer-up-card) (< (best-total customer-hand-so-far) 12))) #t
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

;; If the strategy provided applied to the current hand returns false then check if that strategy applied to the but-last of the current hand is also false.
;; If that the bl of the current hand returns false as well, you should have stopped there but recklessly drew another card. Thus that hand is a reckless hand. Return false. No need to be stupid.
;; If it's true, then you know you should have gone for the next card according to the original strategy, and you were not reckless to hit again.
;; Finally, if the strategy provided on the current hand returned true in the first place, then obviously just hit again.

(define (reckless strategy)
  (lambda (customer-hand-so-far dealer-up-card) (if (equal? (strategy customer-hand-so-far dealer-up-card) #f) (if (equal? (strategy (bl customer-hand-so-far) dealer-up-card) #f) #f
														   #t)
						    #t)))

;;=================================================================
