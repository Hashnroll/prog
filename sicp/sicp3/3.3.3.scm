(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

;;two-dim
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
	      (set-cdr! table
			(cons (cons key-2 value) (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

;;local tables
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys table)
      (if (null? keys)
	  (cdr table)
	  (let ((subtable (assoc (car keys) (cdr table))))
	    (if subtable
		(lookup (cdr keys) subtable)
		false))))
    (define (insert! keys value table)
      (if (null? keys)
	  (set-cdr! table value)
	  (let ((subtable (assoc (car keys) (cdr table))))
	    (if subtable
		(insert! (cdr keys) value subtable)
		(let ((new-table (list (car keys))))
		  (set-cdr! table (cons new-table (cdr table)))
		  (insert! (cdr keys) value new-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys)
				    (lookup keys local-table)))
	    ((eq? m 'insert-proc!) (lambda (keys value)
				     (insert! keys value local-table)))
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(math * = +) '11)
(get '(math * = +))

;;3.25 done
