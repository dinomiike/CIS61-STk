(define (reverse b)
  (if (null? b)
      b
      (append (reverse (cdr b)) (list (car b)))))


;;Disregard this, it doesn't do what we want
;;(define (reverse b)
;;  (define (reverse-iter b counter)
;;    (if (= (length b) counter) (car b)
;;	(list (list-ref b (- (length b) counter)) (reverse-iter b (+ counter 1)))))
;;  (reverse-iter b 1))
