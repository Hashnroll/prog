					;helpers
(define (pow b exp)
  (define (iter a b exp)
    (if (= exp 0) 
	a
	(if (= (remainder exp 2) 0)
	    (iter a (square b) (/ exp 2))
	    (iter (* a b) b (- exp 1)))))
  (iter 1 b exp))
(define (gcd a b)
  (if (= b 0)
      a 
      (gcd b (remainder a b))))

;task
(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (car z)
  (if (= (remainder z 3) 0)
      (car (/ z 3))
      (display (log_a 2 z))))

(define (cdr z)
  (if (= (remainder z 2) 0)
      (cdr (/ z 2))
      (display (log_a 3 z))))

(define (time f start-time)
  (lambda (x) (f x))
  (newline)
  (display (- (runtime) start-time))
  (newline))
	   
(time (cdr (cons 20 213)) (runtime))

