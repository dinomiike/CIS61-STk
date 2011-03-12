(define (same-parity i . x)
  (if (even? i) (append (list i) (filter even? x))
      (append (list i) (filter odd? x))))
