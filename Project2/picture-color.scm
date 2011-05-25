;; Code for CS61 project 2 -- picture language
(load "~/Documents/Projects/lisp/CIS61-STk/Project2/picture.scm")
(load "~/Documents/Projects/lisp/CIS61-STk/Project2/colorpic.scm")

;; Part C
(define draw-diamond (color-segments->painter
(list
(cons 'cyan
(list
(make-segment (make-vect 0.20 1) (make-vect 0.80 1))
(make-segment (make-vect 0.20 1) (make-vect 0 0.65))
(make-segment (make-vect 0 0.65) (make-vect 0.5 0))
(make-segment (make-vect 0.5 0) (make-vect 1 0.65))
(make-segment (make-vect 1 0.65) (make-vect 0.80 1))
(make-segment (make-vect 0 0.65) (make-vect 1 0.65))
(make-segment (make-vect 0.20 1) (make-vect 0.10 0.65))
(make-segment (make-vect 0.10 0.65) (make-vect 0.5 0))
(make-segment (make-vect 0.80 1) (make-vect 0.90 0.65))
(make-segment (make-vect 0.90 0.65) (make-vect 0.5 0))
;; Top shading line
(make-segment (make-vect 0.22 0.98) (make-vect 0.15 0.68))
(make-segment (make-vect 0.78 0.98) (make-vect 0.86 0.68))
(make-segment (make-vect 0.15 0.68) (make-vect 0.86 0.68))
;; Bottom shading line
(make-segment (make-vect 0.15 0.62) (make-vect 0.86 0.62))
(make-segment (make-vect 0.15 0.62) (make-vect 0.49 0.05))
(make-segment (make-vect 0.86 0.62) (make-vect 0.51 0.05))
)))))
