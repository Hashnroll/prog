(define (fast-mult a b)
  (define (fast-iter prod a b)
    (cond ((= b 0) prod)
	  ((even? b) (fast-iter prod (* a 2) (/ b 2)))
	  (else (fast-iter (+ prod a) a (- b 1)))))
  (fast-iter 0 a b))

(fast-mult 9 10)
