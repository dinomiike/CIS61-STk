;;; logo-meta.scm      Part of programming project #4

;;; Differences between the book and this version:  Eval and apply have
;;; been changed to logo-eval and logo-apply so as not to overwrite the Scheme
;;; versions of these routines. An extra procedure initialize-logo has been
;;; added. This routine resets the global environment and then executes the
;;; driver loop. This procedure should be invoked to start the Logo
;;; evaluator executing.  Note: It will reset your global environment and all
;;; definitions to the Logo interpreter will be lost. To restart the Logo
;;; interpreter without resetting the global environment, just invoke
;;; driver-loop.  Don't forget that typing control-C will get you out of
;;; the Logo evaluator back into Scheme.

;;; Problems A1, A2, and B2 are entirely in logo.scm
;;; Problems 3, 7, and up require you to find and change existing procedures.

;;;  Procedures that you must write from scratch:

;;; Problem B1    eval-line

(define (eval-line line-obj env)
  (if (ask line-obj 'empty?) '=no-value=
      (let ((temp (logo-eval line-obj env)))
	(if (equal? temp '=no-value=)
	    (eval-line line-obj env)
	    temp))))

;;; Problem 4    variables  (other procedures must be modified, too)
;;; data abstraction procedures

(define (variable? input)
  (eq? (first input) ':))

(define (variable-name exp)
  (bf exp))


;;; Problem A5   handle-infix

(define (de-infix token)
  (cdr (assoc token '((+ . sum)
		      (- . difference)
		      (* . product)
		      (/ . quotient)
		      (= . equalp)
		      (< . lessp)
		      (> . greaterp)))))

;;(define (handle-infix value line-obj env)
;;  value)   ;; This doesn't give an error message, so other stuff works.
(define (handle-infix value line-obj env)
  (if (ask line-obj 'empty?) value
      (let ((token (ask line-obj 'next)))
	(if (and (not (pair? token))
		 (member? token '(+ - * / = < >)))
	    (handle-infix (eval-prefix (make-line-obj (list (de-infix token) value (eval-prefix line-obj env))) env) line-obj env)
	    (begin
	      (ask line-obj 'put-back token)
	      value)))))


;;; Problem B5    eval-definition

;;(define (eval-definition line-obj)
;;  (define (formal-args lst)
;;    (if (ask line-obj 'empty?) lst
;;	(let ((next (ask line-obj 'next)))
;;	  (if (eq? next 'static) lst
;;	      (begin
;;		(ask line-obj 'put-back next)
;;		(formal-args (append lst (list (variable-name (ask line-obj 'next))))) )))))
;;  (define (static-vars lst)
;;    (if (ask line-obj 'empty?) lst
;;	(static-vars (cons (cons (variable-name (ask line-obj 'next))
;;				 (logo-eval line-obj env))
;;			   lst))))
;;  (define (end? line)
;;    (and (not (null? line))
;;         (eq? (car line) 'end)
;;         (null? (cdr line))))
;;  (define (proc-loop body)
;;    (prompt "-> ")
;;    (let ((line (logo-read)))
;;      (if (end? line)
;;          (let ((name (ask line-obj 'next))
;;                (formals (formal-args '()))
;;		(statics (static-vars '())))
;;	    (add-proc (make-proc name
;;				 (length formals)
;;				 formals
;;				 body
;;				 (make-frame
;;				  (map car statics)
;;				  (map cdr statics))))
;;	    (logo-print (word name " defined")))
;;          (proc-loop (append body (list line))))))
;;  (if (ask line-obj 'empty?) (error "to: no procedure name given")
;;      (proc-loop '()))
;;  '=no-value=)
(define (eval-definition line-obj)
  (define (list-para)
    (if (ask line-obj 'empty?)
            '()
            (let ((para (ask line-obj 'next)))
          (if (eq? para 'static)
              '()
              (append (list (bf para)) (list-para))))))

  (define (interactive-loop)
    (prompt "->")
    (let ((line (list (logo-read))))
      (if (equal? line (list '[end]))
          '()
          (append line (interactive-loop)))))

  (define (get-statics frame)
    (if (ask line-obj 'empty?)
        frame
        (let ((var (bf (ask line-obj 'next)))
               (val (logo-eval line-obj the-global-environment)))
          (add-binding-to-frame! var val frame)
          (get-statics frame))))
  (let ((name (ask line-obj 'next))
        (formals (list-para))
        (body (interactive-loop))
        (statics (get-statics (make-frame '() '()))))
    (set! the-procedures
          (cons (list name 'compound (length formals) (cons formals body) statics)
                the-procedures))
    '=no-value=))
	;______________________________________________________________

(define (procedure-frame proc) (list-ref proc 5))

(define (make-proc name argc formals body frame)
  (list name
	'compound ;; Applies only to user-defined functions (obviously)
	argc ;; The argment names for the procedure
	(cons formals body) ;; The body of the procedure
	#f ;; The step-flag, by default set to false
	frame))

(define (add-proc proc)
  (set! the-procedures
	(cons proc the-procedures)))

;;(define (eval-definition line-obj)
;;  (define (formal-para)
;;    (define (check token)
;;      (if (eq? (first token) ':)
;;	  (bf token)
;;	  (error "Bad input")))
;;    (if (ask line-obj 'empty?)
;;	'()
;;	(let ((token (ask line-obj 'next)))
;;	  (cons (check token) (formal-para))))
;;    (define (body)
;;      (prompt "-> ")
;;      (let ((line (logo-read)))
;;	(if (equal? line '(END))
;;	    '()
;;	    (cons line (body)))))
;;    (let ((name (ask line-obj 'next)))
;;      (let ((formals (formal-para)))
;;	(set! proc (cons (list name 'compound (length formals)
;;			       (cons formals (body)))
;;			 proc))))
;;    '=no-value=))

;;; Problem 6    eval-sequence

;;(define (eval-sequence exps env)
;;  (define tempval (eval-line (make-line-obj (car exps)) env))
;;  (cond ((null? exps) '=no-value=)
;;	((and (pair? tempval) (eq? (car tempval) '=output=)) (cdr tempval))
;;	((eq? tempval '=stop=) '=no-value=)
;;	((not (eq? tempval '=no-value=))
;;	 (error "You don't say what to do with" tempval))
;;	(else (eval-sequence (cdr exps) env))))

(define (eval-sequence exps env stp)
  (cond ((null? exps) '=no-value=)
	((not (null? exps))
	 (if (equal? stp #t) (begin
			       (logo-type (car exps))
			       (prompt ">>> ")
			       (logo-read)))
	 (let ((tempval (eval-line (make-line-obj (car exps)) env)))
	   (cond
	    ((and (pair? tempval) (eq? (car tempval) '=output=)) (cdr tempval))
	    ((eq? tempval '=stop=) '=no-value=)
	    ((not (eq? tempval '=no-value=))
	     (error "You don't say what to do with" tempval))
	    (else (eval-sequence (cdr exps) env stp)))))))

(define (step proc-name)
  (let ((prc (lookup-procedure proc-name)))
    (if (compound-procedure? prc) (set-car! (step-flag prc) #t))
    '=no-value=))

(define (unstep proc-name)
  (let ((prc (lookup-procedure proc-name)))
    (if (compound-procedure? prc) (set-car! (step-flag prc) #f))
    '=no-value=))

;;; SETTING UP THE ENVIRONMENT

(define the-primitive-procedures '())

(define (add-prim name count proc)
  (set! the-primitive-procedures
	(cons (list name 'primitive count proc)
	      the-primitive-procedures)))

(add-prim 'first 1 first)
(add-prim 'butfirst 1 bf)
(add-prim 'bf 1 bf)
(add-prim 'last 1 last)
(add-prim 'butlast 1 bl)
(add-prim 'bl 1 bl)
(add-prim 'word -2 word)
(add-prim 'sentence -2 se)
(add-prim 'se -2 se)
(add-prim 'list -2 list)
(add-prim 'fput 2 cons)

(add-prim 'sum -2 (make-logo-arith +))
(add-prim 'difference 2 (make-logo-arith -))
(add-prim '=unary-minus= 1 (make-logo-arith -))
(add-prim '- 1 (make-logo-arith -))
(add-prim 'product -2 (make-logo-arith *))
(add-prim 'quotient 2 (make-logo-arith /))
(add-prim 'remainder 2 (make-logo-arith remainder))

(add-prim 'print 1 logo-print)
(add-prim 'pr 1 logo-print)
(add-prim 'show 1 logo-show)
(add-prim 'type 1 logo-type)
(add-prim 'make '(2) make)

(add-prim 'run '(1) run)
(add-prim 'if '(2) logo-if)
(add-prim 'ifelse '(3) ifelse)
(add-prim 'equalp -2 (logo-pred (make-logo-arith equalp)))
(add-prim 'equalp3 3 (logo-pred (make-logo-arith equalp3)))
(add-prim 'equalp4 4 (logo-pred (make-logo-arith equalp4)))
(add-prim 'lessp 2 (logo-pred (make-logo-arith <)))
(add-prim 'greaterp 2 (logo-pred (make-logo-arith >)))
(add-prim 'emptyp 1 (logo-pred empty?))
(add-prim 'numberp 1 (logo-pred (make-logo-arith number?)))
(add-prim 'listp 1 (logo-pred list?))
(add-prim 'wordp 1 (logo-pred (lambda (x) (not (list? x)))))

(add-prim 'stop 0 (lambda () '=stop=))
(add-prim 'output 1 (lambda (x) (cons '=output= x)))
(add-prim 'op 1 (lambda (x) (cons '=output= x)))
(add-prim 'step 1 step)
(add-prim 'unstep 1 unstep)

(define (pcmd proc) (lambda args (apply proc args) '=no-value=))
(add-prim 'cs 0 (pcmd cs))
(add-prim 'clearscreen 0 (pcmd cs))
(add-prim 'fd 1 (pcmd fd))
(add-prim 'forward 1 (pcmd fd))
(add-prim 'bk 1 (pcmd bk))
(add-prim 'back 1 (pcmd bk))
(add-prim 'lt 1 (pcmd lt))
(add-prim 'left 1 (pcmd lt))
(add-prim 'rt 1 (pcmd rt))
(add-prim 'right 1 (pcmd rt))
(add-prim 'setxy 2 (pcmd setxy))
(add-prim 'setx 1 (lambda (x) (setxy x (ycor)) '=no-value=))
(add-prim 'sety 1 (lambda (y) (setxy (xcor) y) '=no-value=))
(add-prim 'xcor 0 xcor)
(add-prim 'ycor 0 ycor)
(add-prim 'pos 0 pos)
(add-prim 'seth 1 (pcmd setheading))
(add-prim 'setheading 1 (pcmd setheading))
(add-prim 'heading 0 heading)
(add-prim 'st 0 (pcmd st))
(add-prim 'showturtle 0 (pcmd st))
(add-prim 'ht 0 (pcmd ht))
(add-prim 'hideturtle 0 (pcmd ht))
(add-prim 'shown? 0 shown?)
(add-prim 'pd 0 (pcmd pendown))
(add-prim 'pendown 0 (pcmd pendown))
(add-prim 'pu 0 (pcmd penup))
(add-prim 'penup 0 (pcmd penup))
(add-prim 'pe 0 (pcmd penerase))
(add-prim 'penerase 0 (pcmd penerase))
(add-prim 'home 0 (pcmd home))
(add-prim 'setpc 1 (pcmd setpc))
(add-prim 'setpencolor 1 (pcmd setpc))
(add-prim 'pc 0 pc)
(add-prim 'pencolor 0 pc)
(add-prim 'setbg 1 (pcmd setbg))
(add-prim 'setbackground 1 (pcmd setbg))

(add-prim 'test '(1) test)
(add-prim 'iftrue '(1) iftrue)
(add-prim 'iffalse '(1) iffalse)

(add-prim 'load 1 meta-load)

(define the-global-environment '())
(define the-procedures the-primitive-procedures)



;;; INITIALIZATION AND DRIVER LOOP

;;; The following code initializes the machine and starts the Logo
;;; system.  You should not call it very often, because it will clobber
;;; the global environment, and you will lose any definitions you have
;;; accumulated.

(define (initialize-logo)
  (set! the-global-environment (extend-environment '() '() '()))
  (set! the-procedures the-primitive-procedures)
  (driver-loop))

(define (driver-loop)
  (define (helper)
    (prompt "? ")
    (let ((line (logo-read)))
      (if (not (null? line))
  	  (let ((result (eval-line (make-line-obj line)
				   the-global-environment)))
	    (if (not (eq? result '=no-value=))
		(logo-print (list "You don't say what to do with" result))))))
    (helper))
  (logo-read)
  (helper))

;;; APPLYING PRIMITIVE PROCEDURES

;;; To apply a primitive procedure, we ask the underlying Scheme system
;;; to perform the application.  (Of course, an implementation on a
;;; low-level machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (text p) args))


;;; Now for the code that's based on the book!!!


;;; Section 4.1.1

;; Given an expression like (proc :a :b :c)+5
;; logo-eval calls eval-prefix for the part in parentheses, and then
;; handle-infix to check for and process the infix arithmetic.
;; Eval-prefix is comparable to Scheme's eval.

(define (logo-eval line-obj env)
  (handle-infix (eval-prefix line-obj env) line-obj env))

(define (eval-prefix line-obj env)
  (define (eval-helper paren-flag)
    (let ((token (ask line-obj 'next)))
      (cond ((self-evaluating? token) token)
            ((variable? token)
             (lookup-variable-value (variable-name token) env))
            ((quoted? token) (text-of-quotation token))
            ((definition? token) (eval-definition line-obj))
            ((left-paren? token)
             (let ((result (handle-infix (eval-helper #t)
                                         line-obj
                                         env)))
               (let ((token (ask line-obj 'next)))
                 (if (right-paren? token)
                     result
                     (error "Too much inside parens")))))
            ((right-paren? token)
             (error "Unexpected ')'"))
            (else
             (let ((proc (lookup-procedure token)))
               (if (not proc) (error "I don't know how  to " token))


               (let ((argues '())
		     (i (arg-count proc)))
                 (cond ((pair? i)
                        (set! argues
			      (cons env (collect-n-args (car i) line-obj env))))
                       ((and (eq? paren-flag #t) (< i 0))
                        (set! argues (collect-n-args i line-obj env)))
                       ((< i 0)
                        (set! argues (collect-n-args (abs i) line-obj env)))
                       (else (set! argues (collect-n-args i line-obj env))))
                 (logo-apply proc argues env)))) )))
  (eval-helper #f))
;*****EDIT PRIMITIVE TABLE FOR WORD LIST SUM PRODUCT by adding -
;;(define (logo-apply procedure arguments env)
;;  (cond ((primitive-procedure? procedure)
;;         (apply-primitive-procedure procedure arguments))
;;        ((compound-procedure? procedure)
;;	 (eval-sequence
;;	  (procedure-body procedure)
;;	  (extend-environment
;;	   (frame-variables (static-frame procedure))
;;	   (frame-values (static-frame procedure))
;;	   (extend-environment
;;	    (parameters procedure)
;;	    arguments
;;	    env))
;;	  (step? procedure)))
;;        (else
;;         (error "Unknown procedure type -- LOGO-APPLY " procedure))))

;;(define (logo-apply procedure arguments env)
;;  (cond ((primitive-procedure? procedure)
;;         (apply-primitive-procedure procedure arguments))
;;        ((compound-procedure? procedure)
;;         (let ((body (procedure-body procedure))
;;               (formals (parameters procedure))
;;               (args (cdr arguments))
;;               (env (car arguments))
;;               (static-vars (frame-variables (static-frame procedure)))
;;               (static-vals (frame-values (static-frame procedure))))
;;           (eval-sequence
;;             body
;;	     args
;;	     (extend-environment
;;	      (cons (static-frame procedure) env))
;;	     (step? procedure)))
;;	 (else
;;	  (error "Unknown procedure type -- LOGO-APPLY " procedure)))))

;;(define (logo-apply procedure arguments env)
;;  (cond ((primitive-procedure? procedure)
;;         (apply-primitive-procedure procedure arguments))
;;        ((compound-procedure? procedure)
;;         (let ((body (procedure-body procedure))
;;               (formals (parameters procedure))
;;               (args (cdr arguments))
;;               (env (car arguments))
;;               (static-vars (frame-variables (static-frame procedure)))
;;               (static-vals (frame-values (static-frame procedure))))
;;           (eval-sequence
;;             body
;;             (extend-environment
;;               static-vars
;;               static-vals
;;               (extend-environment
;;                 formals
;;                 args
;;                 env))
;;             (stepped-proc? procedure))))
;;        (else
;;         (error "Unknown procedure type -- LOGO-APPLY " procedure))))

;;
(define (logo-apply procedure arguments env)

	(cond ((primitive-procedure? procedure)

	     (apply-primitive-procedure procedure arguments))

   ((compound-procedure? procedure)

	(eval-sequence (procedure-body procedure)

			(extend-environment (parameters procedure) arguments
						(cons (static-frame procedure) env)
			 )
(step? procedure)
	)
     )

 (else
         (error "Unknown procedure type -- LOGO-APPLY " procedure))))

(define (static-frame p)
  (car (cddddr p)))






(define (collect-n-args n line-obj env)
  (cond ((= n 0) '())
	((and (< n 0) (not (ask line-obj 'empty?)))
	 (let ((token (ask line-obj 'next)))
	   (ask line-obj 'put-back token)
	   (if (right-paren? token)
	       '()
      	       (let ((next (logo-eval line-obj env)))
        	 (cons next
	      	       (collect-n-args (- n 1) line-obj env)) ))))
	(else
      	 (let ((next (logo-eval line-obj env)))
           (cons next
	      	 (collect-n-args (- n 1) line-obj env)) ))))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; quote

(define (quoted? exp)
  (or (list? exp)
      (eq? (string-ref (word->string (first exp)) 0) #\")))

(define (text-of-quotation exp)
  (if (list? exp)
      exp
      (bf exp)))

;;; parens

(define (left-paren? exp) (eq? exp left-paren-symbol))

(define (right-paren? exp) (eq? exp right-paren-symbol))

;;; definitions

(define (definition? exp)
  (eq? exp 'to))

;;; procedures

(define (lookup-procedure name)
  (assoc name the-procedures))

(define (primitive-procedure? p)
  (eq? (cadr p) 'primitive))

(define (compound-procedure? p)
  (eq? (cadr p) 'compound))

(define (arg-count proc)
  (caddr proc))

(define (text proc)
  (cadddr proc))

(define (parameters proc) (car (text proc)))

(define (procedure-body proc) (cdr (text proc)))

(define (step-flag p)
  (cddddr p))

(define (step? p)
  (car (step-flag p)))

;; Access to the static variables set in the frame
(define (static-frame p)
  ;;(car (cddddr p)))
  (car (cddddr p)))

;;; Section 4.1.3

;;; Operations on environments

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;(define (make-frame variables values)
;;  (cons variables values))
;; Problem 12, inserting " test" value to every frame
(define (make-frame variables values)
  (cons
   (cons '" test" variables)
   (cons 'nil values)))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied " vars vals)
          (error "Too few arguments supplied " vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
