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

;; ================================================================================================
;; Exercise 2.44

(define (up-split painter n)
  (if (= n 0) painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;; ================================================================================================
;; Exercise 2.45

;;(define (split a b)
;;  (define (helper a b painter n)
;;    (lambda (painter n) (if (= n 0) painter
;;			     (let ((smaller (helper painter (- n 1))))
;;			       (a painter (b smaller smaller))))))
;;  (lambda (painter n) (helper a b painter n)))

(define (split a b)
  (lambda (painter n) (if (= n 0) painter
			  (let ((smaller ((split a b) painter (- n 1))))
			    (a painter (b smaller smaller))))))

;; ================================================================================================
;; Exercise 2.46

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
;; Exercise 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;;(define (make-frame origin edge1 edge2)
;;  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (make-vect (xcor-vect (car frame)) (ycor-vect (car frame))))

(define (edge1-frame frame)
  (make-vect (xcor-vect (cadr frame)) (ycor-vect (cadr frame))))

(define (edge2-frame frame)
  (if (list? (cddr frame)) (make-vect (xcor-vect (caddr frame)) (ycor-vect (caddr frame)))
      (make-vect (xcor-vect (cddr frame)) (ycor-vect (cddr frame)))))

;; ================================================================================================
;; Exercise 2.48

(define (make-segment s e)
  (cons s e))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; ================================================================================================

;; 2.49
;; Part A
(define outline-frame (segments->painter
		       (list
			(make-segment (make-vect 0 1) (make-vect 0 0))
			(make-segment (make-vect 0 1) (make-vect 1 1))
			(make-segment (make-vect 1 1) (make-vect 1 0))
			(make-segment (make-vect 0 0) (make-vect 1 0)))))

;; Part B
(define draw-x (segments->painter
		(list
		 (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0))
		 (make-segment (make-vect 1.0 1.0) (make-vect 0.0 0.0)))))

;; Part C
(define draw-diamond (segments->painter
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
		       )))

;; Part D - I'm drawing _all_ lines in this from left to right
(define draw-george (segments->painter
		     (list
		      ;; Head
		      (make-segment (make-vect 0.35 0.875) (make-vect 0.40 1))
		      (make-segment (make-vect 0.35 0.875) (make-vect 0.40 0.75))
		      (make-segment (make-vect 0.60 0.75) (make-vect 0.65 0.875))
		      (make-segment (make-vect 0.60 1) (make-vect 0.65 0.875))
		      ;; Left Shoulder
		      (make-segment (make-vect 0.35 0.75) (make-vect 0.4 0.75))
		      ;; Right Shoulder
		      (make-segment (make-vect 0.6 0.75) (make-vect 0.65 0.75))
		      ;; Left Arm
		      (make-segment (make-vect 0.25 0.625) (make-vect 0.35 0.75))
		      (make-segment (make-vect 0 0.875) (make-vect 0.25 0.625))
		      (make-segment (make-vect 0 0.775) (make-vect 0.25 0.5))
		      (make-segment (make-vect 0.25 0.5) (make-vect 0.35 0.68))
		      ;; Right Arm
		      (make-segment (make-vect 0.65 0.75) (make-vect 1 0.25))
		      (make-segment (make-vect 0.65 0.6) (make-vect 1 0.125))
		      ;; Left Body
		      (make-segment (make-vect 0.35 0.68) (make-vect 0.375 0.6))
		      (make-segment (make-vect 0.25 0) (make-vect 0.375 0.6))
		      ;; Legs
		      ;;(make-segment (make-vect 0.35 0) (make-vect 0.5 0.30))
		      (make-segment (make-vect 0.4 0) (make-vect 0.5 0.30))
		      ;;(make-segment (make-vect 0.5 0.30) (make-vect 0.65 0))
		      (make-segment (make-vect 0.5 0.30) (make-vect 0.6 0))
		      ;; Right Body
		      (make-segment (make-vect 0.65 0.6) (make-vect 0.75 0))
		      ;; Helpers
		      ;; Cross-hairs
		      ;;(make-segment (make-vect 0.5 0.6) (make-vect 0.5 0.4))
		      ;;(make-segment (make-vect 0.6 0.5) (make-vect 0.4 0.5))
		      ;; Center Points
		      ;;(make-segment (make-vect 0 0.5) (make-vect 1 0.5))
		      ;;(make-segment (make-vect 0.5 1) (make-vect 0.5 0))
		      ;; Vertical Middle Bar
		      ;;(make-segment (make-vect 0.35 1) (make-vect 0.35 0))
		      ;;(make-segment (make-vect 0.65 1) (make-vect 0.65 0))
		      ;;(make-segment (make-vect 0.4 1) (make-vect 0.4 0))
		      ;;(make-segment (make-vect 0.6 1) (make-vect 0.6 0))
		      ;; Body Line 
		      ;;(make-segment (make-vect 0 0.6) (make-vect 1 0.6)) 
		      )))

(define draw-base (segments->painter
		   (list
		    (make-segment (make-vect 0.0 0.5) (make-vect 1.0 0.5))
		    (make-segment (make-vect 0.5 1.0) (make-vect 0.5 0.0)))))

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

;; Part A -- Updated draw-george (draw-george2) below draw-dino
(define draw-dino (segments->painter
		   (list
		    ;; Top Teeth
		    (make-segment (make-vect 0.059 0.5304) (make-vect 0.0538 0.4592))
		    (make-segment (make-vect 0.0538 0.4592) (make-vect 0.0859 0.5339))
		    (make-segment (make-vect 0.0859 0.5339) (make-vect 0.0851 0.4965))
		    (make-segment (make-vect 0.0851 0.4965) (make-vect 0.0981 0.4557))
		    (make-segment (make-vect 0.0981 0.4557) (make-vect 0.1172 0.4392))
		    (make-segment (make-vect 0.1172 0.4392) (make-vect 0.1102 0.4861))
		    (make-segment (make-vect 0.1102 0.4861) (make-vect 0.1163 0.5339))
		    (make-segment (make-vect 0.1163 0.5339) (make-vect 0.1198 0.4826))
		    (make-segment (make-vect 0.1198 0.4826) (make-vect 0.1415 0.4418))
		    (make-segment (make-vect 0.1415 0.4418) (make-vect 0.1484 0.5356))
		    (make-segment (make-vect 0.1484 0.5356) (make-vect 0.1519 0.4661))
		    (make-segment (make-vect 0.1519 0.4661) (make-vect 0.1667 0.4358))
		    (make-segment (make-vect 0.1667 0.4358) (make-vect 0.1788 0.5035))
		    (make-segment (make-vect 0.1788 0.5035) (make-vect 0.1782 0.4647))
		    (make-segment (make-vect 0.1782 0.4647) (make-vect 0.191 0.4427))
		    (make-segment (make-vect 0.191 0.4427) (make-vect 0.1953 0.487))
		    (make-segment (make-vect 0.1953 0.487) (make-vect 0.2092 0.5156))
		    (make-segment (make-vect 0.2092 0.5156) (make-vect 0.2066 0.4838))
		    (make-segment (make-vect 0.2066 0.4838) (make-vect 0.2161 0.4497))
		    (make-segment (make-vect 0.2161 0.4497) (make-vect 0.224 0.4826))
		    (make-segment (make-vect 0.224 0.4826) (make-vect 0.2378 0.5012))
		    (make-segment (make-vect 0.2378 0.5012) (make-vect 0.2387 0.4609))
		    (make-segment (make-vect 0.2387 0.4609) (make-vect 0.2509 0.441))
		    (make-segment (make-vect 0.2509 0.441) (make-vect 0.2656 0.4835))
		    (make-segment (make-vect 0.2656 0.4835) (make-vect 0.2648 0.4557))
		    (make-segment (make-vect 0.2648 0.4557) (make-vect 0.2743 0.4254))
		    (make-segment (make-vect 0.2743 0.4254) (make-vect 0.2847 0.4583))
		    (make-segment (make-vect 0.2847 0.4583) (make-vect 0.2995 0.467))
		    (make-segment (make-vect 0.2995 0.467) (make-vect 0.2908 0.4462))
		    (make-segment (make-vect 0.2908 0.4462) (make-vect 0.303 0.4097))
		    (make-segment (make-vect 0.303 0.4097) (make-vect 0.3108 0.4358))
		    (make-segment (make-vect 0.3108 0.4358) (make-vect 0.3264 0.4497))
		    (make-segment (make-vect 0.3264 0.4497) (make-vect 0.3212 0.4335))
		    (make-segment (make-vect 0.3212 0.4335) (make-vect 0.3351 0.4097))
		    (make-segment (make-vect 0.3351 0.4097) (make-vect 0.3489 0.4369))
		    (make-segment (make-vect 0.3489 0.4369) (make-vect 0.3576 0.4028))
		    (make-segment (make-vect 0.3576 0.4028) (make-vect 0.3741 0.4384))
		    (make-segment (make-vect 0.3741 0.4384) (make-vect 0.388 0.4062))
		    (make-segment (make-vect 0.388 0.4062) (make-vect 0.3993 0.4375))
		    (make-segment (make-vect 0.3993 0.4375) (make-vect 0.4132 0.4132))
		    (make-segment (make-vect 0.4132 0.4132) (make-vect 0.4236 0.4436))
		    (make-segment (make-vect 0.4236 0.4436) (make-vect 0.4297 0.4236))
		    (make-segment (make-vect 0.4297 0.4236) (make-vect 0.4479 0.4479))
		    (make-segment (make-vect 0.4479 0.4479) (make-vect 0.4566 0.4201))
		    (make-segment (make-vect 0.4566 0.4201) (make-vect 0.4714 0.4601))
		    (make-segment (make-vect 0.4714 0.4601) (make-vect 0.4783 0.4297))
		    (make-segment (make-vect 0.4783 0.4297) (make-vect 0.4896 0.4688))
		    (make-segment (make-vect 0.4896 0.4688) (make-vect 0.5017 0.4392))
		    (make-segment (make-vect 0.5017 0.4392) (make-vect 0.5069 0.4774))
		    (make-segment (make-vect 0.5069 0.4774) (make-vect 0.5269 0.4505))
		    (make-segment (make-vect 0.5269 0.4505) (make-vect 0.5304 0.4939))
		    (make-segment (make-vect 0.5304 0.4939) (make-vect 0.5408 0.4627))
		    (make-segment (make-vect 0.5408 0.4627) (make-vect 0.553 0.5095))

		    ;; Bottom Teeth
		    (make-segment (make-vect 0.1675 0.3898) (make-vect 0.1727 0.4193))
		    (make-segment (make-vect 0.1727 0.4193) (make-vect 0.1823 0.3898))
		    (make-segment (make-vect 0.1823 0.3898) (make-vect 0.1892 0.4158))
		    (make-segment (make-vect 0.1892 0.4158) (make-vect 0.1988 0.3898))
		    (make-segment (make-vect 0.1988 0.3898) (make-vect 0.2075 0.4201))
		    (make-segment (make-vect 0.2075 0.4201) (make-vect 0.2161 0.3898))
		    (make-segment (make-vect 0.2161 0.3898) (make-vect 0.2266 0.4158))
		    (make-segment (make-vect 0.2266 0.4158) (make-vect 0.2405 0.388))
		    (make-segment (make-vect 0.2405 0.388) (make-vect 0.2517 0.4132))
		    (make-segment (make-vect 0.2517 0.4132) (make-vect 0.2561 0.3837))
		    (make-segment (make-vect 0.2561 0.3837) (make-vect 0.2734 0.4019))
		    (make-segment (make-vect 0.2734 0.4019) (make-vect 0.2769 0.3811))
		    (make-segment (make-vect 0.2769 0.3811) (make-vect 0.2908 0.3993))
		    (make-segment (make-vect 0.2908 0.3993) (make-vect 0.2986 0.3715))
		    (make-segment (make-vect 0.2986 0.3715) (make-vect 0.3073 0.3984))
		    (make-segment (make-vect 0.3073 0.3984) (make-vect 0.316 0.3707))
		    (make-segment (make-vect 0.316 0.3707) (make-vect 0.3307 0.3915))
		    (make-segment (make-vect 0.3307 0.3915) (make-vect 0.3411 0.3655))
		    (make-segment (make-vect 0.3411 0.3655) (make-vect 0.3455 0.388))
		    (make-segment (make-vect 0.3455 0.388) (make-vect 0.362 0.3707))
		    (make-segment (make-vect 0.362 0.3707) (make-vect 0.3628 0.3984))
		    (make-segment (make-vect 0.3628 0.3984) (make-vect 0.3845 0.3785))
		    (make-segment (make-vect 0.3845 0.3785) (make-vect 0.3881 0.4045))
		    (make-segment (make-vect 0.3881 0.4045) (make-vect 0.4045 0.3889))
		    (make-segment (make-vect 0.4045 0.3889) (make-vect 0.4045 0.4132))
		    (make-segment (make-vect 0.4045 0.4132) (make-vect 0.4306 0.3932))
		    (make-segment (make-vect 0.4306 0.3932) (make-vect 0.4349 0.4219))
		    (make-segment (make-vect 0.4349 0.4219) (make-vect 0.4583 0.4089))
		    (make-segment (make-vect 0.4583 0.4089) (make-vect 0.4696 0.4358))
		    (make-segment (make-vect 0.4696 0.4358) (make-vect 0.4931 0.421))
		    (make-segment (make-vect 0.4931 0.421) (make-vect 0.4925 0.4497))
		    (make-segment (make-vect 0.4925 0.4497) (make-vect 0.5162 0.4282))
		    (make-segment (make-vect 0.5162 0.4282) (make-vect 0.5324 0.4774))
		    (make-segment (make-vect 0.5324 0.4774) (make-vect 0.5573 0.4497))
		    
		    ;; Eye
		    (make-segment (make-vect 0.6042 0.8134) (make-vect 0.585 0.7928))
		    (make-segment (make-vect 0.585 0.7928) (make-vect 0.5877 0.7752))
		    (make-segment (make-vect 0.5877 0.7752) (make-vect 0.5998 0.7595))
		    (make-segment (make-vect 0.5998 0.7595) (make-vect 0.6293 0.7691))
		    (make-segment (make-vect 0.6293 0.7691) (make-vect 0.6389 0.7812))
		    (make-segment (make-vect 0.6389 0.7812) (make-vect 0.6233 0.8125))
		    (make-segment (make-vect 0.6233 0.8125) (make-vect 0.6042 0.8134))

		    ;; Nose
		    (make-segment (make-vect 0.2266 0.6536) (make-vect 0.2274 0.6615))
		    (make-segment (make-vect 0.2274 0.6615) (make-vect 0.2179 0.6658))
		    (make-segment (make-vect 0.2179 0.6658) (make-vect 0.2049 0.6597))
		    (make-segment (make-vect 0.2049 0.6597) (make-vect 0.1944 0.6493))
		    (make-segment (make-vect 0.1944 0.6493) (make-vect 0.1875 0.6319))
		    (make-segment (make-vect 0.1875 0.6319) (make-vect 0.1844 0.625))

		    ;; Eye Frame
		    (make-segment (make-vect 0.6927 0.803) (make-vect 0.7031 0.7986))
		    (make-segment (make-vect 0.7031 0.7986) (make-vect 0.7075 0.8038))
		    (make-segment (make-vect 0.7075 0.8038) (make-vect 0.704 0.8186))
		    (make-segment (make-vect 0.704 0.8186) (make-vect 0.7075 0.8255))
		    (make-segment (make-vect 0.7075 0.8255) (make-vect 0.7066 0.8411))
		    (make-segment (make-vect 0.7066 0.8411) (make-vect 0.7135 0.849))
		    (make-segment (make-vect 0.7135 0.849) (make-vect 0.7049 0.8585))
		    (make-segment (make-vect 0.7049 0.8585) (make-vect 0.6944 0.8628))
		    (make-segment (make-vect 0.6944 0.8628) (make-vect 0.6892 0.8689))
		    (make-segment (make-vect 0.6892 0.8689) (make-vect 0.6832 0.8785))
		    (make-segment (make-vect 0.6832 0.8785) (make-vect 0.6684 0.8776))
		    (make-segment (make-vect 0.6684 0.8776) (make-vect 0.6415 0.8785))
		    (make-segment (make-vect 0.6415 0.8785) (make-vect 0.6155 0.8698))
		    (make-segment (make-vect 0.6155 0.8698) (make-vect 0.6111 0.8559))
		    (make-segment (make-vect 0.6111 0.8559) (make-vect 0.592 0.8481))
		    (make-segment (make-vect 0.592 0.8481) (make-vect 0.5851 0.8377))
		    (make-segment (make-vect 0.5851 0.8377) (make-vect 0.5582 0.8108))
		    (make-segment (make-vect 0.5582 0.8108) (make-vect 0.533 0.7865))
		    (make-segment (make-vect 0.533 0.7865) (make-vect 0.5052 0.7726))
		    (make-segment (make-vect 0.5052 0.7726) (make-vect 0.4627 0.7535))
		    (make-segment (make-vect 0.4627 0.7535) (make-vect 0.4314 0.7405))
		    (make-segment (make-vect 0.4314 0.7405) (make-vect 0.4002 0.7266))
		    (make-segment (make-vect 0.4002 0.7266) (make-vect 0.3802 0.7153))
		    (make-segment (make-vect 0.3802 0.7153) (make-vect 0.3507 0.6875))
		    (make-segment (make-vect 0.3507 0.6875) (make-vect 0.3264 0.6615))
		    (make-segment (make-vect 0.3264 0.6615) (make-vect 0.3151 0.6441))
		    (make-segment (make-vect 0.3151 0.6441) (make-vect 0.316 0.6224))
		    (make-segment (make-vect 0.316 0.6224) (make-vect 0.3351 0.5998))
		    (make-segment (make-vect 0.3351 0.5998) (make-vect 0.362 0.5894))
		    (make-segment (make-vect 0.362 0.5894) (make-vect 0.3863 0.5825))
		    (make-segment (make-vect 0.3863 0.5825) (make-vect 0.4201 0.5842))
		    (make-segment (make-vect 0.4201 0.5842) (make-vect 0.4332 0.5903))
		    (make-segment (make-vect 0.4332 0.5903) (make-vect 0.447 0.5877))
		    (make-segment (make-vect 0.447 0.5877) (make-vect 0.4818 0.6007))
		    (make-segment (make-vect 0.4818 0.6007) (make-vect 0.5009 0.6267))
		    (make-segment (make-vect 0.5009 0.6267) (make-vect 0.5139 0.658))
		    (make-segment (make-vect 0.5139 0.658) (make-vect 0.5113 0.7005))
		    (make-segment (make-vect 0.5113 0.7005) (make-vect 0.5043 0.7231))
		    (make-segment (make-vect 0.5043 0.7231) (make-vect 0.4896 0.7396))
		    (make-segment (make-vect 0.4896 0.7396) (make-vect 0.4766 0.737))
		    (make-segment (make-vect 0.4766 0.737) (make-vect 0.4818 0.7127))
		    (make-segment (make-vect 0.4818 0.7127) (make-vect 0.487 0.6484))
		    (make-segment (make-vect 0.487 0.6484) (make-vect 0.4722 0.6068))
		    
		    (make-segment (make-vect 0.4766 0.737) (make-vect 0.4653 0.7413))
		    (make-segment (make-vect 0.4653 0.7413) (make-vect 0.4514 0.7335))
		    (make-segment (make-vect 0.4514 0.7335) (make-vect 0.4392 0.7231))
		    (make-segment (make-vect 0.4392 0.7231) (make-vect 0.4436 0.691))
		    (make-segment (make-vect 0.4436 0.691) (make-vect 0.447 0.6493))
		    (make-segment (make-vect 0.447 0.6493) (make-vect 0.4462 0.6163))
		    (make-segment (make-vect 0.4462 0.6163) (make-vect 0.4323 0.599))
		    
		    (make-segment (make-vect 0.4392 0.7231) (make-vect 0.4253 0.724))
		    (make-segment (make-vect 0.4253 0.724) (make-vect 0.4149 0.7188))
		    (make-segment (make-vect 0.4149 0.7188) (make-vect 0.4002 0.7179))
		    (make-segment (make-vect 0.4002 0.7179) (make-vect 0.3854 0.6997))
		    (make-segment (make-vect 0.3854 0.6997) (make-vect 0.3837 0.684))
		    (make-segment (make-vect 0.3837 0.684) (make-vect 0.3872 0.6623))
		    (make-segment (make-vect 0.3872 0.6623) (make-vect 0.3793 0.6137))
		    (make-segment (make-vect 0.3793 0.6137) (make-vect 0.375 0.6007))
		    
		    (make-segment (make-vect 0.3854 0.6997) (make-vect 0.3715 0.6936))
		    (make-segment (make-vect 0.3715 0.6936) (make-vect 0.349 0.6788))
		    
		    ;; Inner eye
		    (make-segment (make-vect 0.6155 0.8099) (make-vect 0.605 0.803))
		    (make-segment (make-vect 0.605 0.803) (make-vect 0.5981 0.7891))
		    (make-segment (make-vect 0.5981 0.7891) (make-vect 0.5972 0.7804))
		    (make-segment (make-vect 0.5972 0.7804) (make-vect 0.6016 0.7708))
		    (make-segment (make-vect 0.6016 0.7708) (make-vect 0.6089 0.7643))
		    
		    ;; Under eye
		    (make-segment (make-vect 0.5851 0.7917) (make-vect 0.5668 0.7769))
		    (make-segment (make-vect 0.5668 0.7769) (make-vect 0.5591 0.7649))
		    (make-segment (make-vect 0.5591 0.7649) (make-vect 0.5417 0.7378))
		    (make-segment (make-vect 0.5417 0.7378) (make-vect 0.546 0.6936))
		    (make-segment (make-vect 0.546 0.6936) (make-vect 0.5703 0.6493))
		    (make-segment (make-vect 0.5703 0.6493) (make-vect 0.612 0.6059))
		    (make-segment (make-vect 0.612 0.6059) (make-vect 0.6519 0.5825))
		    (make-segment (make-vect 0.6519 0.5825) (make-vect 0.7023 0.579))
		    (make-segment (make-vect 0.7023 0.579) (make-vect 0.7352 0.592))
		    (make-segment (make-vect 0.7352 0.592) (make-vect 0.7587 0.6102))
		    (make-segment (make-vect 0.7587 0.6102) (make-vect 0.7743 0.6424))
		    (make-segment (make-vect 0.7743 0.6424) (make-vect 0.7977 0.6892))
		    
		    (make-segment (make-vect 0.5477 0.7387) (make-vect 0.553 0.7205))
		    (make-segment (make-vect 0.553 0.7205) (make-vect 0.5582 0.7005))
		    (make-segment (make-vect 0.5582 0.7005) (make-vect 0.5911 0.6806))
		    (make-segment (make-vect 0.5911 0.6806) (make-vect 0.658 0.6484))
		    (make-segment (make-vect 0.658 0.6484) (make-vect 0.684 0.6424))
		    (make-segment (make-vect 0.684 0.6424) (make-vect 0.7057 0.6476))
		    (make-segment (make-vect 0.7057 0.6476) (make-vect 0.7804 0.7161))
		    (make-segment (make-vect 0.7804 0.7161) (make-vect 0.7908 0.7326))
		    (make-segment (make-vect 0.7908 0.7326) (make-vect 0.7891 0.7535))
		    (make-segment (make-vect 0.7891 0.7535) (make-vect 0.7821 0.7865))
		    (make-segment (make-vect 0.7821 0.7865) (make-vect 0.776 0.8177))
		    (make-segment (make-vect 0.776 0.8177) (make-vect 0.7752 0.8446))
		    (make-segment (make-vect 0.7752 0.8446) (make-vect 0.7769 0.8628))
		    (make-segment (make-vect 0.7769 0.8628) (make-vect 0.7648 0.8924))
		    
		    (make-segment (make-vect 0.5582 0.7569) (make-vect 0.559 0.7457))
		    (make-segment (make-vect 0.559 0.7457) (make-vect 0.5694 0.7274))
		    (make-segment (make-vect 0.5694 0.7274) (make-vect 0.6085 0.6953))
		    
		    (make-segment (make-vect 0.5703 0.7734) (make-vect 0.5729 0.7474))
		    (make-segment (make-vect 0.5729 0.7474) (make-vect 0.6007 0.7169))
		    (make-segment (make-vect 0.6007 0.7169) (make-vect 0.6233 0.7023))
		    (make-segment (make-vect 0.6233 0.7023) (make-vect 0.645 0.697))
		    (make-segment (make-vect 0.645 0.697) (make-vect 0.6701 0.7101))
		    (make-segment (make-vect 0.6701 0.7101) (make-vect 0.6884 0.7292))
		    (make-segment (make-vect 0.6884 0.7292) (make-vect 0.6962 0.7578))
		    (make-segment (make-vect 0.6962 0.7578) (make-vect 0.6979 0.7969))
		    
		    (make-segment (make-vect 0.4384 0.7595) (make-vect 0.4835 0.7786))
		    (make-segment (make-vect 0.4835 0.7786) (make-vect 0.5069 0.7917))
		    (make-segment (make-vect 0.5069 0.7917) (make-vect 0.5217 0.8073))
		    (make-segment (make-vect 0.5217 0.8073) (make-vect 0.5425 0.829))
		    (make-segment (make-vect 0.5425 0.829) (make-vect 0.5747 0.8568))
		    (make-segment (make-vect 0.5747 0.8568) (make-vect 0.6128 0.8854))
		    (make-segment (make-vect 0.6128 0.8854) (make-vect 0.6406 0.8958))
		    (make-segment (make-vect 0.6406 0.8958) (make-vect 0.7075 0.8993))
		    (make-segment (make-vect 0.7075 0.8993) (make-vect 0.7257 0.8516))
		    (make-segment (make-vect 0.7257 0.8516) (make-vect 0.7205 0.803))
		    (make-segment (make-vect 0.7205 0.803) (make-vect 0.7066 0.7448))

		    (make-segment (make-vect 0.5781 0.7821) (make-vect 0.5842 0.7613))
		    (make-segment (make-vect 0.5842 0.7613) (make-vect 0.6059 0.7439))
		    (make-segment (make-vect 0.6059 0.7439) (make-vect 0.6484 0.7387))
		    (make-segment (make-vect 0.6484 0.7387) (make-vect 0.6745 0.7457))
		    (make-segment (make-vect 0.6745 0.7457) (make-vect 0.6858 0.7726))
		    
		    ;; Mouth piece
		    (make-segment (make-vect 0.5642 0.4444) (make-vect 0.5868 0.4375))
		    (make-segment (make-vect 0.5868 0.4375) (make-vect 0.6094 0.4236))
		    (make-segment (make-vect 0.6094 0.4236) (make-vect 0.6259 0.4123))
		    (make-segment (make-vect 0.6259 0.4123) (make-vect 0.6424 0.3941))
		    (make-segment (make-vect 0.6424 0.3941) (make-vect 0.6571 0.3924))
		    (make-segment (make-vect 0.6571 0.3924) (make-vect 0.6675 0.3836))
		    (make-segment (make-vect 0.6675 0.3836) (make-vect 0.6849 0.3863))
		    (make-segment (make-vect 0.6849 0.3863) (make-vect 0.6892 0.3915))
		    
		    (make-segment (make-vect 0.5677 0.5269) (make-vect 0.5981 0.5312))
		    (make-segment (make-vect 0.5981 0.5312) (make-vect 0.6372 0.5078))
		    (make-segment (make-vect 0.6372 0.5078) (make-vect 0.6719 0.4714))
		    (make-segment (make-vect 0.6719 0.4714) (make-vect 0.671 0.428))
		    (make-segment (make-vect 0.671 0.428) (make-vect 0.6753 0.408))
		    (make-segment (make-vect 0.6753 0.408) (make-vect 0.6875 0.3958))
		    (make-segment (make-vect 0.6875 0.3958) (make-vect 0.6997 0.3958))
		    (make-segment (make-vect 0.6997 0.3958) (make-vect 0.7352 0.4123))
		    (make-segment (make-vect 0.7352 0.4123) (make-vect 0.7908 0.467))
		    (make-segment (make-vect 0.7908 0.467) (make-vect 0.8646 0.4913))
		    (make-segment (make-vect 0.8646 0.4913) (make-vect 0.8828 0.4948))
		    (make-segment (make-vect 0.8828 0.4948) (make-vect 0.875 0.4175))
		    (make-segment (make-vect 0.875 0.4175) (make-vect 0.8602 0.3802))
		    (make-segment (make-vect 0.8602 0.3802) (make-vect 0.842 0.3342))
		    (make-segment (make-vect 0.842 0.3342) (make-vect 0.809 0.2951))
		    (make-segment (make-vect 0.809 0.2951) (make-vect 0.7804 0.2691))
		    (make-segment (make-vect 0.7804 0.2691) (make-vect 0.7309 0.2378))
		    (make-segment (make-vect 0.7309 0.2378) (make-vect 0.7023 0.2274))
		    (make-segment (make-vect 0.7023 0.2274) (make-vect 0.6589 0.2231))
		    (make-segment (make-vect 0.6589 0.2231) (make-vect 0.5825 0.2422))
		    (make-segment (make-vect 0.5825 0.2422) (make-vect 0.5009 0.2639))
		    
		    ;; T-Rex Head Outline
		    (make-segment (make-vect 0.4887 0.934) (make-vect 0.4722 0.9306))
		    (make-segment (make-vect 0.4722 0.9306) (make-vect 0.4566 0.9201))
		    (make-segment (make-vect 0.4566 0.9201) (make-vect 0.4444 0.9002))
		    (make-segment (make-vect 0.4444 0.9002) (make-vect 0.4358 0.8741))
		    (make-segment (make-vect 0.4358 0.8741) (make-vect 0.4219 0.8628))
		    (make-segment (make-vect 0.4219 0.8628) (make-vect 0.4028 0.8707))
		    (make-segment (make-vect 0.4028 0.8707) (make-vect 0.3637 0.8516))
		    (make-segment (make-vect 0.3637 0.8516) (make-vect 0.355 0.8429))
		    (make-segment (make-vect 0.355 0.8429) (make-vect 0.342 0.8429))
		    (make-segment (make-vect 0.342 0.8429) (make-vect 0.171 0.7639))
		    (make-segment (make-vect 0.171 0.7639) (make-vect 0.145 0.7431))
		    (make-segment (make-vect 0.145 0.7431) (make-vect 0.1181 0.7127))
		    (make-segment (make-vect 0.1181 0.7127) (make-vect 0.0668 0.6068))
		    (make-segment (make-vect 0.0668 0.6068) (make-vect 0.0495 0.5434))
		    (make-segment (make-vect 0.0495 0.5434) (make-vect 0.0547 0.5321))
		    (make-segment (make-vect 0.0547 0.5321) (make-vect 0.0816 0.5391))
		    (make-segment (make-vect 0.0816 0.5391) (make-vect 0.1684 0.5391))
		    (make-segment (make-vect 0.1684 0.5391) (make-vect 0.2361 0.5069))
		    (make-segment (make-vect 0.2361 0.5069) (make-vect 0.3394 0.4479))
		    (make-segment (make-vect 0.3394 0.4479) (make-vect 0.3941 0.4384))
		    (make-segment (make-vect 0.3941 0.4384) (make-vect 0.4592 0.4549))
		    (make-segment (make-vect 0.4592 0.4549) (make-vect 0.5243 0.4939))
		    (make-segment (make-vect 0.5243 0.4939) (make-vect 0.5634 0.5217))
		    (make-segment (make-vect 0.5634 0.5217) (make-vect 0.5599 0.4462))
		    (make-segment (make-vect 0.5599 0.4462) (make-vect 0.4523 0.3967))
		    (make-segment (make-vect 0.4523 0.3967) (make-vect 0.3464 0.3576))
		    (make-segment (make-vect 0.3464 0.3576) (make-vect 0.1997 0.3854))
		    (make-segment (make-vect 0.1997 0.3854) (make-vect 0.158 0.3837))
		    (make-segment (make-vect 0.158 0.3837) (make-vect 0.1424 0.3698))
		    (make-segment (make-vect 0.1424 0.3698) (make-vect 0.1441 0.3255))
		    (make-segment (make-vect 0.1441 0.3255) (make-vect 0.1675 0.2865))
		    (make-segment (make-vect 0.1675 0.2865) (make-vect 0.2509 0.2595))
		    (make-segment (make-vect 0.2509 0.2595) (make-vect 0.3828 0.2526))
		    (make-segment (make-vect 0.3828 0.2526) (make-vect 0.4514 0.2648))
		    (make-segment (make-vect 0.4514 0.2648) (make-vect 0.4957 0.2604))
		    (make-segment (make-vect 0.4957 0.2604) (make-vect 0.526 0.2335))
		    (make-segment (make-vect 0.526 0.2335) (make-vect 0.5781 0.151))
		    (make-segment (make-vect 0.5781 0.151) (make-vect 0.7144 0.1736))
		    (make-segment (make-vect 0.7144 0.1736) (make-vect 0.8394 0.2509))
		    (make-segment (make-vect 0.8394 0.2509) (make-vect 0.9097 0.3151))
		    (make-segment (make-vect 0.9097 0.3151) (make-vect 0.9323 0.3681))
		    (make-segment (make-vect 0.9323 0.3681) (make-vect 0.9401 0.4323))
		    (make-segment (make-vect 0.9401 0.4323) (make-vect 0.9523 0.4861))
		    (make-segment (make-vect 0.9523 0.4861) (make-vect 0.9583 0.5868))
		    (make-segment (make-vect 0.9583 0.5868) (make-vect 0.9297 0.717))
		    (make-segment (make-vect 0.9297 0.717) (make-vect 0.8915 0.8446))
		    (make-segment (make-vect 0.8915 0.8446) (make-vect 0.8576 0.8915))
		    (make-segment (make-vect 0.8576 0.8915) (make-vect 0.7648 0.8958))
		    (make-segment (make-vect 0.7648 0.8958) (make-vect 0.7483 0.9115))
		    (make-segment (make-vect 0.7483 0.9115) (make-vect 0.7092 0.9115))
		    (make-segment (make-vect 0.7092 0.9115) (make-vect 0.7031 0.9253))
		    (make-segment (make-vect 0.7031 0.9253) (make-vect 0.6745 0.9366))
		    (make-segment (make-vect 0.6745 0.9366) (make-vect 0.5825 0.9271))
		    (make-segment (make-vect 0.5825 0.9271) (make-vect 0.5061 0.9132))
		    (make-segment (make-vect 0.5061 0.9132) (make-vect 0.4896 0.9193))
		    (make-segment (make-vect 0.4896 0.9193) (make-vect 0.4887 0.934))
		    )))

(define draw-george2 (segments->painter
		     (list
		      ;; Head
		      (make-segment (make-vect 0.35 0.875) (make-vect 0.40 1))
		      (make-segment (make-vect 0.35 0.875) (make-vect 0.40 0.75))
		      (make-segment (make-vect 0.60 0.75) (make-vect 0.65 0.875))
		      (make-segment (make-vect 0.60 1) (make-vect 0.65 0.875))
		      ;; Left Shoulder
		      (make-segment (make-vect 0.35 0.75) (make-vect 0.4 0.75))
		      ;; Right Shoulder
		      (make-segment (make-vect 0.6 0.75) (make-vect 0.65 0.75))
		      ;; Left Arm
		      (make-segment (make-vect 0.25 0.625) (make-vect 0.35 0.75))
		      (make-segment (make-vect 0 0.875) (make-vect 0.25 0.625))
		      (make-segment (make-vect 0 0.775) (make-vect 0.25 0.5))
		      (make-segment (make-vect 0.25 0.5) (make-vect 0.35 0.68))
		      ;; Right Arm
		      (make-segment (make-vect 0.65 0.75) (make-vect 1 0.25))
		      (make-segment (make-vect 0.65 0.6) (make-vect 1 0.125))
		      ;; Left Body
		      (make-segment (make-vect 0.35 0.68) (make-vect 0.375 0.6))
		      (make-segment (make-vect 0.25 0) (make-vect 0.375 0.6))
		      ;; Legs
		      (make-segment (make-vect 0.4 0) (make-vect 0.5 0.30))
		      (make-segment (make-vect 0.5 0.30) (make-vect 0.6 0))
		      ;; Right Body
		      (make-segment (make-vect 0.65 0.6) (make-vect 0.75 0))
		      ;; Smile
		      (make-segment (make-vect 0.43 0.83) (make-vect 0.45 0.80))
		      (make-segment (make-vect 0.45 0.80) (make-vect 0.49 0.80))
		      (make-segment (make-vect 0.49 0.80) (make-vect 0.5 0.81))
		      (make-segment (make-vect 0.5 0.81) (make-vect 0.51 0.80))
		      (make-segment (make-vect 0.51 0.80) (make-vect 0.55 0.80))
		      (make-segment (make-vect 0.55 0.80) (make-vect 0.57 0.83))
		      ;; Left Eye
		      (make-segment (make-vect 0.42 0.87) (make-vect 0.435 0.87))
		      (make-segment (make-vect 0.42 0.87) (make-vect 0.42 0.86))
		      (make-segment (make-vect 0.435 0.87) (make-vect 0.435 0.86))
		      (make-segment (make-vect 0.42 0.86) (make-vect 0.435 0.86))
		      ;; Right Eye
		      (make-segment (make-vect 0.56 0.87) (make-vect 0.575 0.87))
		      (make-segment (make-vect 0.56 0.87) (make-vect 0.56 0.86))
		      (make-segment (make-vect 0.575 0.87) (make-vect 0.575 0.86))
		      (make-segment (make-vect 0.56 0.86) (make-vect 0.575 0.86))
		      
		      ;; Helpers
		      ;; Cross-hairs
		      ;;(make-segment (make-vect 0.5 0.6) (make-vect 0.5 0.4))
		      ;;(make-segment (make-vect 0.6 0.5) (make-vect 0.4 0.5))
		      ;; Center Points
		      ;;(make-segment (make-vect 0 0.5) (make-vect 1 0.5))
		      ;;(make-segment (make-vect 0.5 1) (make-vect 0.5 0))
		      ;; Vertical Middle Bar
		      ;;(make-segment (make-vect 0.35 1) (make-vect 0.35 0))
		      ;;(make-segment (make-vect 0.65 1) (make-vect 0.65 0))
		      ;;(make-segment (make-vect 0.4 1) (make-vect 0.4 0))
		      ;;(make-segment (make-vect 0.6 1) (make-vect 0.6 0))
		      ;; Body Line 
		      ;;(make-segment (make-vect 0 0.6) (make-vect 1 0.6)) 
		      )))

;; Part B
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; Part C
;; The original square-limit procedure:
;;(define (square-limit painter n)
;;  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
;;    (combine4 (corner-split painter n))))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four identity flip-horiz flip-vert rotate180)))
    (combine4 (corner-split painter n))))

;; ================================================================================================

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))




