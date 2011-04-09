;; Source from the book
;; element-of-set? is a predicate that determines whether a given element is a member of a set
;; adjoin-set takes an object and a set as arguments and returns a set that contains the elements of the original set and also the adjoined element
;; union-set computers the union of two sets, which is the set containing each element that appears in either argument
;; intersection-set computes the intersection of two sets, which is the set containing only elements that appear in both arguments

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59. Implement the union-set operation for the unordered-list of sets.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
	(else (union-set (cdr set1) (cons (car set1) set2)))))

;; or

(define (union-set2 set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

;; or

(define (union-set3 set1 set2)
  (append set1 (filter (lambda (x) (not (element-of-set? x set1))) set2)))
