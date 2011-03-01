(define (ordered? sent)
  (if (= (count sent) 1) #t
      (if (< (first sent) (first (bf sent))) (ordered? (bf sent))
      #f)))
