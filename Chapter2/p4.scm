(define (ends-e sent)
  (define (e? wd)
    (if (equal? (last wd) 'e) wd
	'()))
  (every e? sent))
