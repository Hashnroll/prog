(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))    
	    (map proc (cdr items)))))

(define (map-iter proc items)
  (define (iter items res)
    (if (null? items)
	res
	(iter (cdr items) (append res (list (proc (car items)))))))
  (iter items ()))
;
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items))
	    (square-list (cdr items)))))
(define (square-list items)
  (map-iter square items))
(square-list (list 1 2 3 4 5))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items ()))

ex2.23
(define (for-each proc items)
  (cond ((null? items)
	 #t)
	(else (proc (car items))
	      (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 1 2 3 4 5))





