(define (try a b)
  (if (= a 0) 1 b))

(try 0 (/ 1 0))

(if (= 0 0) 0 (/ 1 0))

(define (unless cond us-val except-val)
  (if cond except-val us-val))

(define b 0)
(define a 2)

(unless (= b 0)
	(/ a b)
	(begin (display "exception: returning 0")
	       0))

;;ex4.26
(define (unless cond us-val except-val)
  
