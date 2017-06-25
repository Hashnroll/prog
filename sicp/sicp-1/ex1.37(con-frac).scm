;recursive
(define (con-frac n d k)
  (define (inc-arg i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (inc-arg (+ i 1))))))
  (inc-arg 1.0))

;iterative
(define (con-frac n d k)
  (define (iter i res)
    (if (= i 0)
	res
	(iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0.0))

(con-frac (lambda (i) i)
	  (lambda (i) (* i i))
	  11)

