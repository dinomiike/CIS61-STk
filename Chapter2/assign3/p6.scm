(define (substitute your-list old new)
  (define (check-it x)
    (if (word? x) (if (equal? x old) new
		      x)
	(map check-it x)))
  (map check-it your-list))
