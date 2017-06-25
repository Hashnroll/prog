(define (identity x)
  x)

(define (inc x)
  (+ x 1))

;
(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
	res
	(iter (next a) (combiner (term a)
				 res))))
  (iter a null-value))

(define (fact n)
  (accumulate * 1 identity 1 inc n))

(define (sum-of-squares n)
  (accumulate + 0 square 1 inc n))

;ex1.43
(define (repeated f n)
  (accumulate compose 
	      (lambda (x) x)
	      (lambda (i) f)
	      1 inc n))

((repeated square 2) 5)
