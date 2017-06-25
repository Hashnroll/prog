(define (prime? n)
  (define (prime-iter a)
    (if (> (square a) n)
	#t
	(if (= (remainder n a) 0)
	    #f
	    (prime-iter (+ a 1)))))
  (if (= n 1)
      #f
      (prime-iter 2)))

(define (filter-accumulate predicate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
	res
	(if (predicate a)
	    (iter (next a) (combiner (term a) res))
	    (iter (next a) res))))
  (iter a null-value))

;a
(define (sum-of-squares-prime a b)
  (filter-accumulate prime? + 0 square a inc b))

(sum-of-squares-prime 1 5)

;b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relative-primes n)
  (define (relative-prime? a)
    (= 1 (gcd n a)))
  (filter-accumulate relative-prime? * 1 identity 1 inc (- n 1)))

(product-of-relative-primes 10)
  

