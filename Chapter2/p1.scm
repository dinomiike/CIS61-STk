(define (filter w)
  (cond ((equal? w 'i) 'you2)
	((equal? w 'me) 'you2)
	(else w)))

(define (filter2 w)
  (cond ((equal? w 'you) 'me)
	((equal? w 'you2) 'you)
	(else w)))

(define (switch sent)
  (if (equal? (item 1 sent) 'you) (se 'I (bf (every filter2 (every filter sent))))
      (every filter2 (every filter sent))))
