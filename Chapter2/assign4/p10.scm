(define (fringe x)
  (cond ((null? x) nil)
	((not (list? x)) (list x))
	(else (append (fringe (car x))
		      (fringe (cdr x))))))
