(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (even? x)
  (= 0 (remainder x 2)))

(define (full-fermat-test n)
  (define (test-iter a)
    (if (< a (- n 1))
	(if (= (expmod a (- n 1) n) 1)
	    (test-iter (+ a 1))
	    #f)
	#t))
  (test-iter 2))

(full-fermat-test 17)