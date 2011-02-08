;; Aaron Ramos
;; CIS61 Lab2
;; 2/7/11
(define (lab pt item)
  (if (empty? item) 0
      (if (equal? pt (first item))
	  (+ 1 (lab pt (bf item)))
	  (lab pt (bf item)))
      )
  )
