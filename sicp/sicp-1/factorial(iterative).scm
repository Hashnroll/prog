(define (factorial n)
  (define (factorial-iter product counter)
    (if (> counter n)
	product
	(factorial-iter (* product counter) 
			(+ counter 1))))
  (factorial-iter 1 1))

(factorial 16)