;; Assignment 6 - Exercise 3.25

;; Original 1-Dimensional Procedures
(define (lookup1d key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert1d! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;;=============================================================
;; Original 2-Dimensional Procedures
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

;;(define (insert2! keys value table)
;;  (let ((subtable (assoc (car keys) (cdr table))))
;;    (if subtable
;;        (let ((record (assoc (cdr keys) (cdr subtable))))
;;          (if record
;;              (set-cdr! record value)
;;              (set-cdr! subtable
;;                        (cons (cons (cadr keys) value)
;;                              (cdr subtable)))))
;;        (set-cdr! table
;;                  (cons (list (car keys)
;;                              (cons (cadr keys) value))
;;                        (cdr table)))))
;;  'ok)


(define (insert3! keys value table)
  (let ((subitem (assoc (car keys) (cdr table))))
    (if subitem
	;; Match
	(if (> (length keys) 1) (begin
				  (insert3! (cdr keys) value subitem))
	    (begin
	      (set-cdr! subitem value)))
	;; No Match
	(if (> (length keys) 1) (begin
				  (set-cdr! table (cons
						   (list (car keys))
						   (cdr table)))
				  (insert3! (cdr keys) value (cadr table)))
	    (begin
	      (set-cdr! table (cons
			       (cons (car keys)
				     value)
			       (cdr table)))))))
  'ok)



;;=====================
;;====================
