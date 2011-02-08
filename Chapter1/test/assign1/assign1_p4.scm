;; Problem 4

(define (palindrome? w)
        (if (> (count w) 1)
                (if (equal? (first w) (last w))
                        (palindrome? (bl (bf w)))
                        #f
                )
                #t
        )
)
