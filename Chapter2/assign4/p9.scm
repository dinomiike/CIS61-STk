(define (reverse b)
  (if (null? b)
      b
      (append (reverse (cdr b)) (list (car b)))))

(define (deep-reverse c)
  (cond ((null? c) nil)
	((pair? (car c)) (append (deep-reverse (cdr c))
				     (list (deep-reverse (car c)))))
	(else (append (deep-reverse (cdr c)) (list (car c))))))
