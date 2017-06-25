(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
(last-pair (list ()))

;ex2.18(reverse list)
(define (reverse a)
  (if (null? a)
      a
      (append (reverse (cdr a))
	      (list (car a)))))
	      
(define squares (list 1 4 9 16 25))
(reverse squares)

;ex2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))
(define (first-denomination denominations)
  (car denominations))
(define (except-first-denomination denominations)
  (cdr denominations))
(define (no-more? denominations)
  (null? denominations))

(cc 100 uk-coins)

;ex2.20
(define (even? x)
  (= 0 (remainder x 2)))
(define same-parity
  (lambda (x . l)
    (define (construct source res)
      (if (null? source)
	  res
	  (construct (cdr source) 
		     (if (even? (- x (car source)))
			 (append res (list (car source)))
			 res))))				
    (construct l (list x))))
(same-parity 2 213 51 23 45 12 6)

