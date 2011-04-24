;; Append Vector2 to Vector1
(define (vector-append vect1 vect2)
  (let ((l1 (vector->list vect1))
	(l2 (vector->list vect2)))
    (list->vector (append l1 l2))))
