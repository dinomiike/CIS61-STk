;; Assignment 5

;; Problem 1
(define-class (person name)
  (instance-vars (prev '()))
  (method (say stuff)
	  (set! prev stuff)
	  stuff)
  (method (ask stuff)
	  (set! prev (se '(would you please) stuff))
	  (ask self 'say (se '(would you please) stuff)))
  (method (greet)
	  (set! prev (se '(hello my name is) name))
	  (ask self 'say (se '(hello my name is) name)))
  (method (repeat) prev))
