;; Above the code
(define-class (account balance)
  (method (deposit amount)
	  (set! balance (+ amount balance))
	  balance)
  (method (withdraw amount)
	  (if (< balance amount) "Insufficient funds"
	      (begin
		(set! balance (- balance amount))
		balance)))
  (method (get-balance)
	  (if (> balance 0) balance
	      (se '(Insert coins to continue)))))

(define-class (checking-account init-balance)
  (parent (account init-balance))
  (instance-vars (check-fee 0.10))
  (method (write-check amount)
	  (ask self 'withdraw (+ amount check-fee)))
  (method (set-fee! fee)
	  (set! check-fee fee))
  (method (get-fee)
	  check-fee))

(define-class (worker)
  (instance-vars (hunger 0))
  (class-vars (all-workers '())
	      (work-done 0))
  (initialize (set! all-workers (cons self all-workers)))
  (method (work)
	  (set! hunger (+ hunger 1))
	  (set! work-done (+ work-done 1))
	  'whistle-while-you-work))

(define-class (TA)
  (parent (worker))
  (method (work)
	  (usual 'work)
	  '(Let me help you with that box and pointer diagram))
  (method (grade-exam) 'A+))

(define-class (singer)
  (parent (worker))
  (method (sing) '(fa-lala-la-la)))

(define-class (singer-TA)
  (parent (singer) (TA)))

(define-class (TA-singer)
  (parent (TA) (singer)))

(define-class (echo-previous message)
  (instance-vars (previous-message 'first-time))
  (default-method
    (let ((result previous-message))
      (set! previous-message message)
      result)))
