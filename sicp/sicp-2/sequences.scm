(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n) ()
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (1+ k)))
	      (next (1+ k))))))
    (next 0))
  
;sequence operations
(define (filter predicate sequence)
  (cond ((null? sequence) ())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence) 
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree)) 
	(else (append (enumerate-tree (car tree)) 
		      (enumerate-tree (cdr tree))))))
	
(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square 
		   (filter odd?
			   (enumerate-tree tree)))))

(define x (list 1 (list 2 3 (list 4 5))))
(sum-odd-squares x)

(define (even-fibs n)
  (accumulate cons
	      ()
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))
(even-fibs 1100)
	  





