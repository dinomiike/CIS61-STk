;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (place name)
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (parent (basic-object))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))
    'appeared)
;;;;;;;; Problem A4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (enter new-person)
	  (let ((first-police (filter (lambda (p) (ask p 'police?)) people)))
	    (if (memq new-person people)
		(error "Person already in this place" (list name new-person)))
	    (set! people (cons new-person people))
	    (for-each (lambda (proc) (proc)) entry-procs)
	    (for-each (lambda (pers) (ask pers 'notice new-person))
                  (delete new-person people))
	    (if (> (length first-police) 0) (ask (car first-police) 'inspect new-person))
	'appeared))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)
  ;; Problem B4B
  (method (place?) #t)
  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared)
;;;;;;;; Problem A4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (may-enter? person)
	#t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (default-method
    (ask self 'get message)) )


(define-class (person name place)
  (instance-vars
;;;;;;;; Problem A7a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (money 100)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (possessions '())
   (saying ""))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 10))
  (parent (basic-object))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  ((not (eq? (ask thing 'possessor) 'no-one)) (ask thing 'may-take? self))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions)) 
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	   (ask thing 'change-possessor self)
	   'taken)))
;;;;;;;; Problem A7a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (get-money bank)
	  (set! money (+ money bank)))
  (method (pay-money bank)
	  (cond ((>= money bank)
		 (set! money (- money bank))
		 #t)
		(else #f)))
;;;;;;;; Problem A8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (buy food-name)
	  (let ((food (ask place 'sell self food-name)))
	    (if food
		(begin
		  (set! possessions (cons food possessions))
		  (ask food 'change-possessor self))
		(error "Sorry, you could not buy the food" food-name))))
;;;;;;;; Problem B4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (take-all)
	  (if (null? (ask self 'look-around)) "There is nothing to take"
	      (filter (lambda (thing) (if (and (thing? thing) (eq? (owner thing) 'no-one)) (ask self 'take thing))) (ask (ask self 'place) 'things))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (eat)
	  (let ((items-to-eat (filter (lambda (thing) (if (ask thing 'edible?) #t #f)) (ask self 'possessions))))
	    (let ((cal-items (map (lambda (thing) (ask thing 'calories)) items-to-eat)))
	      (let ((total-cals (reduce + cal-items)))
		(ask self 'put 'strength (+ (ask self 'strength) total-cals))
		(map (lambda (thing) (ask self 'lose thing)) items-to-eat)))))
  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (cond ((null? new-place)
	     (error "Can't go" direction))
;;;;;;;; Problem A4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    ((not (ask new-place 'may-enter? self))
	     (error "Can't enter locked place"
		   (ask new-place 'name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (else
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p)
		(if (ask p 'laptop?)
		    (if (ask place 'hotspot?)
			(if (memq p (ask place 'connected-laptops))
			    (ask place 'gone-hotspot p)))))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self)))))
;;;;;;;; Problem A6a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (method (go-directly-to new-place)
	  (announce-move name place new-place)
	  (for-each (lambda (p)
		      (ask place 'gone p)
		      (ask new-place 'appear p))
		    possessions)
	  (ask place 'exit self)
	  (set! place new-place)
	  (ask new-place 'enter self))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (method (person?) #t) ;; Problem B4b
   (default-method
     (ask self 'get message)))

(define-class (police name place)
  (initialize
   (ask self 'put 'strength 50))
  (parent (person name place))
  (method (type) 'police)
  (method (inspect p)
	  (if (eq? (ask p 'type) 'thief) (begin
					   (display "Crime does not pay")
					   (map (lambda (thing) (ask self 'take thing)) (ask p 'possessions))
					   (map (lambda (thing) (ask p 'lose item)) (ask p 'possessions))
					   (ask p 'go-directly-to jail))))
  (method (police?) #t)
  (default-method
    (ask self 'get message)))

;;;;;;;; Problem 2E ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define-class (thing name)
;;  (instance-vars (possessor 'no-one))
;;  (method (type) 'thing)
;;  (method (change-possessor new-possessor)
;;	  (set! possessor new-possessor)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define thing
;;  (let ()
;;    (lambda (class-message)
;;      (cond
;;       ((eq? class-message 'instantiate)
;;	(lambda (name)
;;	  (let ((self '()) (possessor 'no-one))
;;	    (define (dispatch message)
;;	      (cond
;;	       ((eq? message 'initialize)
;;		(lambda (value-for-self)
;;		  (set! self value-for-self)))
;;	       ((eq? message 'send-usual-to-parent)
;;		(error "Can't use USUAL without a parent." 'thing))
;;	       ((eq? message 'name) (lambda () name))
;;	       ((eq? message 'possessor) (lambda () possessor))
;;	       ((eq? message 'type) (lambda () 'thing))
;;	       ((eq? message 'change-possessor)
;;		(lambda (new-possessor)
;;		  (set! possessor new-possessor)))
;;	       (else (no-method 'thing))))
;;	    dispatch)))
;;       (else (error "Bad message to class" class-message))))))

;;===========================================================================
;; Part 1 - Problem 2E
;;===========================================================================
(define-class (thing name)
  (instance-vars
   (possessor 'no-one))
  (parent (basic-object))
  (method (send-usual-to-parent)
	  (error "Can't use USUAL without a parent." 'thing))
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor)
	  (set! possessor new-possessor))
  (method (thing?) #t)
  (method (may-take? receiver) (if (> (ask receiver 'strength) (ask (ask self 'possessor) 'strength)) self
				   #f))
  (default-method
    (ask self 'get message)))


;;===========================================================================
;; Part 2 - Problem B6
;;===========================================================================
(define-class (food name cal)
  (instance-vars
   (edible? #t))
  (initialize
   (ask self 'put 'calories cal))
  (parent (thing name))
  (method (food?) #t)
  (default-method
    (ask self 'get message)))

(define-class (bagel cal)
  (class-vars
   (name 'bagel))
  (parent (food name cal))
  (method (bagel?) #t)
  (method (type) 'bagel)
  (default-method
    (ask self 'get message)))


(define-class (falafel cal)
  (class-vars
   (name 'falafel))
  (parent (food name cal))
  (method (falafel?) #t)
  (default-method
    (ask self 'get message)))

(define-class (fries cal)
  (class-vars
   (name 'fries))
  (parent (food name cal))
  (method (fries?) #t)
  (default-method
    (ask self 'get message)))



;;;;;;;; Problem 2F ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (whereis person)
  (ask (ask person 'place) 'name))

(define (owner thing)
  (let ((obj (ask thing 'possessor)))
    (if (eq? obj 'no-one)
	'no-one
	(ask obj 'name))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (reload-me)
(define Miike (instantiate person 'Miike Dormitory)))

(define (show-my-stuff person)
  (map (lambda (thing) (ask thing 'name)) (ask person 'possessions)))

;;;;;;;; Problem A3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sproul-hall-exit
  (let ((times 0))
    (lambda ()
      (set! times (1+ times))
      (if (= (remainder times 4) 0)
	  '()
	  (error "Can't exit! Check out!")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; Problem A4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (locked-place name)
  (parent (place name))
  (instance-vars (unlocked? #f))
  (method (unlock)
	  (set! unlocked? #t))
  (method (may-enter? person)
	  unlocked?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem A5

(define-class (garage name)
  (parent (place name))
  (classvars (counter 0))
  (instance-vars (cars (make-table)))
  (method (park car)
	  (if (not (member car (ask self 'things)))
	      (error "Car isn't here!"))
	  (set! counter (+ counter 1))
	  (insert! counter car cars)
	  (let ((ticket (instantiate ticket counter))
		(owner (ask car 'possessor)))
	    (ask self 'appear ticket)
	    (ask owner 'lose car)
	    (ask owner 'take ticket)))
  (method (unpark ticket)
	  (if (equal? (ask ticket 'name) 'ticket)
	      (let* ((number (ask ticket 'number))
		     (car (lookup number cars))
		     (owner (ask ticket 'possessor)))
		(if (equal? car #f)
		    (error "No car parked for ticket" number))
		(insert! number #f cars)
		(ask owner 'lose 'ticket)
		(ask owner 'take car))
	      (error "Not a ticket!" (ask ticket 'name)))))

(define-class (ticket number)
  (parent (thing 'ticket)))

;;;;;;;; Problem A7b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-class (restaurant name food-class price)
  (parent (place name))
  (method (menu) (list (ask food-class 'name) price))
  (method (sell customer order)
	  ;;(if (eq? order (ask food-class 'name))
	  (if (eq? (ask order 'name) (ask (ask self 'food-class) 'name))
	      (if (or (ask customer 'police?) (ask customer 'pay-money price))
		  (let ((food (instantiate food-class 0.50)))
		    (ask self 'appear food)
		    (ask food 'name))
		  #f)
	      #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-class (basic-object)
  (instance-vars (properties (make-table)))
  (method (put property value)
	  (insert! property value properties))
  (method (get property)
	  (lookup property properties)))
        
(define-class (hotspot name pass)
  (instance-vars
   (password pass)
   (connected-laptops '()))
  (parent (place name))
  (method (hotspot?) #t)
  (method (gone-hotspot cl)
	  (if (not (memq cl connected-laptops))
	      (error "Cannot delete a non-item")
	  (set! connected-laptops (delete cl connected-laptops))))
  (method (connect laptop pass)
	  ;; If the password submitted matches the password for this hotspot and the laptop is inside the hotspot, allow connection
	  (if (and (eq? pass password)
		   (eq? self (ask (ask laptop 'possessor) 'place))) (set! connected-laptops (cons laptop connected-laptops))
	      "You cannot connect..."))
  (method (surf laptop url)
	  ;; This will evaluate to #f if the laptop is not connected to the network, otherwise allow surfage
	  (if (memq laptop connected-laptops) (system (string-append "lynx " url))
	      (error "You are not connected to the network")))
  (default-method
    (ask self 'get message)))
    
    
(define-class (laptop name)
  (parent (thing name))
  (method (laptop?) #t)
  (method (connect pass)
	  (ask
	   ;; Possessors place
	   (ask (ask self 'possessor) 'place)
	   'connect self pass))
  (method (surf url)
	  (ask
	   (ask (ask self 'possessor) 'place)
	   'surf self url))
  (default-method
    (ask self 'get message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

;;(define (edible? thing)
;;  (member? (ask thing 'name) *foods*))
(define (edible? thing)
  (ask thing 'edible?))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (initialize
   (ask self 'put 'strength 35))
  (method (type) 'thief)
  (method (notice person)
    (if (eq? behavior 'run) 
;;;;;;;; Problem A6b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let ((exits (ask (usual 'place) 'exits)))
      (if (not (null? exits))
	  (ask self 'go (pick-random exits))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let ((food-things
	   (filter (lambda (thing)
		     (and (edible? thing)
			  (not (eq? (ask thing 'possessor) self))))
		   (ask (usual 'place) 'things))))
      (if (not (null? food-things))
	  (begin
	    (ask self 'take (car food-things))
	    (set! behavior 'run)
	    (ask self 'notice person)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))

(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff))))))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))
