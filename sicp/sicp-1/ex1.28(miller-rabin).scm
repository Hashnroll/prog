(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else #f)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 
	 1)
	((even? exp)
	 (check-nontrivial-sqrt (expmod base (/ exp 2) m) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

(define (check-nontrivial-sqrt n m)
  (if (and (not (= n 1)) (not (= n (- m 1))) (= (remainder (square n) m) 1))
      0
      (remainder (square n) m)))

(define (even? x)
  (= 0 (remainder x 2)))

(fast-prime? 2 5)

;I GOT IT. YEAH, RLY!!!!
				