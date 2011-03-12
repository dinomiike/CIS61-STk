(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; Substitution for car
;; (car (cons 5 8))

;;=(car (lambda (m) (m x y)))
;;=(car (lambda (m) (m 5 8)))
;;=((z (lambda (p q) p)) (lambda (m) (m 5 8)))
;;=((lambda (m) (m 5 8)) (lambda (p q) p))
;;=((lambda (p q) p) 5 8)
;;;; For 5 8, return 5
;;=5

;;=========================

;; Substitution for cdr
;; (cdr (cons 5 8))

;;(cdr (lambda (m) (m x y)))
;;(cdr (lambda (m) (m 5 8)))
;;((z (lambda (p q) q)) (lambda (m) (m 5 8)))
;;((lambda (m) (m 5 8)) (lambda (p q) q))
;;((lambda (p q) q) 5 8)
;;;; For 5 and 8, return 8
;;=8
