(define (square n) (* n n))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
        (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

