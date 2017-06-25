(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (even? x)
  (= 0 (remainder x 2)))

(define (fermat-test n)
  (define (try-it-a a)
    (= (expmod a (- n 1) n) 1))
  (try-it-a (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (prime? n)
  (fast-prime? n 100))

(define (search-for-primes n k)
  (cond ((= k 0) 
	 (display "end"))
	(else (search-for-primes 
	       (+ 1 (searching-first-prime n)) (- k 1)))))

(define (searching-first-prime n)
  (cond ((even? n) (searching-first-prime (+ n 1)))
	((timed-prime-test n) n)
	(else (searching-first-prime (+ n 2)))))
      
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime-time n (- (runtime) start-time))
      #f))

(define (report-prime-time n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

(search-for-primes 6600 1)

;1.23 added
;1.24 added





     

