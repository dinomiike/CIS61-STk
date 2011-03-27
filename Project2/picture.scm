;; Code for CS61 project 2 -- picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;;
;; Your code goes here
;;

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))



;; ================================================================================================

(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;; ================================================================================================

(define (split a b)
  (define (helper a b painter n)
    (lambda (painter n) (if (= n 0) painter
			     (let ((smaller (helper painter (- n 1))))
			       (a painter (b smaller smaller))))))
  (lambda (painter n) (helper a b painter n)))

;; ================================================================================================

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; ================================================================================================

(define (origin-frame frame)
  (make-vect (xcor-vect (car frame)) (ycor-vect (car frame))))

(define (edge1-frame frame)
  (make-vect (xcor-vect (cadr frame)) (ycor-vect (cadr frame))))

(define (edge2-frame frame)
  (if (list? (cddr frame)) (make-vect (xcor-vect (caddr frame)) (ycor-vect (caddr frame)))
      (make-vect (xcor-vect (cddr frame)) (ycor-vect (cddr frame)))))

;; ================================================================================================

(define (make-segment s e)
  (cons s e))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; ================================================================================================

;; 2.48
;; A
(define outline-frame (segments->painter
		       (list
			(make-segment (make-vect 0 1) (make-vect 0 0))
			(make-segment (make-vect 0 1) (make-vect 1 1))
			(make-segment (make-vect 1 1) (make-vect 1 0))
			(make-segment (make-vect 0 0) (make-vect 1 0)))))

;; B
(define draw-x (segments->painter
		(list
		 (make-segment (make-vect 1 0) (make-vect 1 0))
		 (make-segment (make-vect 1 1) (make-vect 0 0)))))

;; C
(define draw-diamond (segments->painter
		      (list
		       (make-segment (make-vect 0.25 1) (make-vect 0.75 1))
		       (make-segment (make-vect 0.25 1) (make-vect 0 0.5))
		       (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
		       (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
		       (make-segment (make-vect 1 0.5) (make-vect 0.75 1))
		       (make-segment (make-vect 0 0.5) (make-vect 1 0.5)))))

;; D
(define draw-george (segments->painter
		     (list
		      (make-segment (make-vect 0.44 0.875) (make-vect 0.505 1))
		      (make-segment (make-vect 0.44 0.875) (make-vect 0.505 0.71875))
		      (make-segment (make-vect 0.375 0.71875) (make-vect 0.505 0.71875))
		      (make-segment (make-vect 0.25 0.625) (make-vect 0.375 0.71875))
		      (make-segment (make-vect 0 0.875) (make-vect 0.25 0.625))
		      (make-segment (make-vect 0 0.75) (make-vect 0.25 0.45))
		      (make-segment (make-vect 0.25 0.45) (make-vect 0.375 0.6875))
		      (make-segment (make-vect 0.375 0.6875) (make-vect 0.44 0.6))
		      (make-segment (make-vect 0.3 0) (make-vect 0.44 0.6))
		      (make-segment (make-vect 0.505 0) (make-vect 0.5 0.35))
		      (make-segment (make-vect 0.5 0.35) (make-vect 0.56 0))
		      (make-segment (make-vect 0.625 0.6) (make-vect 0.7 0))
		      (make-segment (make-vect 0.625 0.6) (make-vect 1 0.25))
		      (make-segment (make-vect 0.69 0.71875) (make-vect 1 0.35))
		      (make-segment (make-vect 0.56 0.71875) (make-vect 0.69 0.71875))
		      (make-segment (make-vect 0.56 0.74875) (make-vect 0.625 0.875))
		      (make-segment (make-vect 0.56 1) (make-vect 0.625 0.875)))))

;; ================================================================================================
;; Exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1 0) ;; new origin
		     (make-vect 0 0) ;; new edge1 corner
		     (make-vect 1 1))) ;; new edge2 corner

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1 1)
		     (make-vect 0 1)
		     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0 1)
		     (make-vect 0 0)
		     (make-vect 1 1)))

;; ================================================================================================
;; Exercise 2.51

;; Part 1
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))

;; Part 2
;;(define (bottom-half painter split-point)
;;  (transform-painter painter
;;		     (make-vect 0.0 0.0)
;;		     (make-vect 1.0 0.0)
;;		     split-point))
;;
;;(define (top-half painter split-point)
;;  (transform-painter painter
;;		     split-point
;;		     (make-vect 0.5 0.5)
;;		     (make-vect 0.0 1.0)))
;;
;;(define (below2 painter1 painter2)
;;  (let ((split-point (make-vect 0.0 0.5)))
;;    (lambda (frame)
;;      (bottom-half painter1 split-point)
;;      (top-half painter2 split-point))))

(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

;; ================================================================================================
;; Exercise 2.52

;; Do it now!
