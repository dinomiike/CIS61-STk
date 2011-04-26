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

;;(define (insert2! keys value table)
;;  (let ((subitem (assoc (car keys) (cdr table))))
;;    (if subitem
;;	(let ((subitem2 (assoc (cdr keys) (cdr subitem))))
;;	  (if subitem2
;;	      "Both keys match"
;;	      "Only first key matches"))
;;	"No match")))

(define (insert2! keys value table)
  (let ((subtable (assoc (car keys) (cdr table))))
    (if subtable
        (let ((record (assoc (cdr keys) (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons (cadr keys) value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list (car keys)
                              (cons (cadr keys) value))
                        (cdr table)))))
  'ok)




;;=====================
;;====================
(*table* (math (+ . 43)))
STk> (define db2 (make-table))
db2
STk> db2
(*table*)
STk> (set-cdr! db2 (cons (list 'math) (cdr db2)))
okay
STk> db2
(*table* (math))
STk> (cdr db2)
((math))
STk> (set-cdr! db2 (cons (list '+ 43) (cdr db2)))
okay
STk> db2
(*table* (+ 43) (math))
STk> (define db2 (make-table))
db2
STk> (set-cdr! db2 (cons (list 'math) (cdr db2)))
okay
STk> db2
(*table* (math))
STk> (set-cdr! db2 (cons (cdr db2) (list (cons '+ 43))))
okay
STk> db2
(*table* ((math)) (+ . 43))
STk> (define db2 (make-table))
db2
STk> db2
(*table*)
STk> (set-cdr! db2 (cons (list 'math) (cdr db2)))
okay
STk> db2
(*table* (math))
STk> (set-cdr! db2 (cons (cons '+ 43) (cadr db2)))
okay
STk> db2
(*table* (+ . 43) math)
STk> newdb
(*table* (math (+ . 43)))
STk> (define db2 (make-table))
db2
STk> (set-cdr! db2 (cons (list 'math) (cdr db2)))
okay
STk> db2
(*table* (math))
STk> (set-cdr! db2 (cons (cons (cadr db2) (cons '+ 43)) (cdr db2)))
okay
STk> db2
(*table* ((math) + . 43) (math))
STk> (define db2 (make-table))
db2
STk> (set-cdr! db2 (cons (list 'math)) (cdr db2))
*** Error:
    eval: Bad number of parameters: (set-cdr! db2 (cons (list (quote math))) (cdr db2))
Current eval stack:
__________________
  0    (set-cdr! db2 (cons (list (quote math))) (cdr db2))
STk> db2
(*table*)
STk> (set-cdr! db2 (cons (list 'math) (cdr db2)))
okay
STk> db2
(*table* (math))
STk> (set-cdr! db2 (cons (cadr db2) (cons '+ 43)))
okay
STk> db2
(*table* (math) + . 43)
STk> newdb
(*table* (math (+ . 43)))
STk> (define db2 (make-table))
db2
STk> (set-cdr! db2 (list 'math))
okay
STk> db2
(*table* math)
STk> (set-cdr! (list 'math) (list (cons '+ 43)))
okay
STk> db2
(*table* math)
STk> (set-cdr! (cdr db2) (list (cons '+ 43)))
okay
STk> db2
(*table* math (+ . 43))
STk> (define db2 (make-table))
db2
STk> (set-cdr! (cddr db2) (cons (cons '+ 43) (cddr db2)))
*** Error:
    cddr: bad list: (*table*)
Current eval stack:
__________________
  0    (cddr db2)
  1    (set-cdr! (cddr db2) (cons (cons (quote +) 43) (cddr db2)))
STk> (set-cdr! (assoc 'math (cdr db2)) (cons (cons '+ 43) (assoc 'math (cdr db2))))
*** Error:
    set-cdr!: wrong type of argument: #f
Current eval stack:
__________________
  0    (set-cdr! (assoc (quote math) (cdr db2)) (cons (cons (quote +) 43) (assoc (quote math) (cdr db2))))
STk> db2
(*table*)
STk> (set-cdr! db2 (cons (list 'math) (cdr db2))
)
okay
STk> db2
(*table* (math))
STk> (set-cdr! (assoc 'math (cdr db2)) (cons (cons '+ 43) (assoc 'math (cdr db2))))
okay
STk> db2
