;; Assignment 6 - Problem 4
;; Don't hate me for this solution.
;; It just says to write vector-filter, but the way I would have written is is like vector-append, which is just like the one supplied.
;; I figure you wanted us to come up with another solution, that is likely O(n) time or something. It's messy but it works.
;; If you run this, please use vector-filter2, not vector-filter. The first time I wrote it, it produced the list in reverse.
;; I'm a packrat so I didn't want to erase it. It could come in handy someday!

(define (vector-append vect1 vect2)
  (let ((l1 (vector->list vect1))
	(l2 (vector->list vect2)))
    (list->vector (append l1 l2))))

(define (vector-map fn vec)
  (define (loop newvec n)
    (if (< n 0)
	newvec
	(begin (vector-set! newvec n (fn (vector-ref vec n)))
	       (loop newvec (- n 1)))))
  (loop (make-vector (vector-length vec)) (- (vector-length vec) 1)))

;; Can ignore this, it returns the list backwards..
(define (vector-filter fn vec)
  (define (loop prev n)
    (if (< n 0)
	prev
	(if (fn (vector-ref vec n)) (if (null? (vector-ref prev (- (vector-length prev) 1))) (loop (vector (vector-ref vec n)) (- n 1))
					(loop (vector-append prev (vector (vector-ref vec n))) (- n 1)))
	    (loop prev (- n 1)))))
  (loop (vector nil) (- (vector-length vec) 1)))


(define (vector-filter2 fn vec)
  (define (loop prev n)
    (if (> n (- (vector-length vec) 1))
	prev
	(if (fn (vector-ref vec n)) (if (null? (vector-ref prev (- (vector-length prev) 1))) (loop (vector (vector-ref vec n)) (+ n 1))
					(loop (vector-append prev (vector (vector-ref vec n))) (+ n 1)))
	    (loop prev (+ n 1)))))
  (loop (vector nil) 0))

;; The running time of vector-filter2 is O(n) where n is the length of the vector.
;; The procedure supplied in problem 4 is O(1)? It runs in constant space because there is no recursion. (However, filter (list) does recurse.. So I'm not sure..)
