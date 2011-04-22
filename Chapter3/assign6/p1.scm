;; Original 1-Dimensional Procedure
;;(define (lookup key table)
;;  (let ((record (assoc key (cdr table))))
;;    (if record
;;	(cdr record)
;;	false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;;(define (insert! key value table)
;;  (let ((record (assoc key (cdr table))))
;;    (if record
;;        (set-cdr! record value)
;;        (set-cdr! table
;;                  (cons (cons key value) (cdr table)))))
;;  'ok)

(define (make-table)
  (list '*table*))

;;=============================================================

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (lookup2 keys table)
  (let ((subitem (assoc (car keys) (cdr table))))
    (if subitem
	;; If this is not a pair, it's a base case -- a record; return the value
	(if (not (pair? (cdr subitem))) (cdr subitem)
	    (lookup2 (cdr keys) subitem))
	;; There is no match
	false)))

(define (insert2! keys value table)
  (if (null? keys) "Stopping")
  (let ((subitem (assoc (car keys) (cdr table))))
    (if subitem
	;; Returns true -- do some stuff
	"The first key is already in the table. Stopping for now"
	;; Returns false -- is it a base case?
	(if (not (pair? (cdr table))) ;;(set-cdr! subitem
						  ;;(cons (cons (car keys) value)
							;;(cdr subitem)))
	    "You have reached the base case -- add to the list"
	    ;; This is not a base case, recurse
	    ;;(insert2! (cdr keys) value subitem))))
	    (begin
	      (cdr keys)
	      (newline)
	      value
	      (newline)
	      (cdr table)
	      (newline)
	      (insert2! (cdr keys) value (cdr table))))))
  ;;'ok)
 )
