(define (sqrt x)
  (define (good-enough? guess oldguess)
    (<= (abs (- guess oldguess)) (* guess 0.00001)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess oldguess)
    (display oldguess) (newline)
    (if (good-enough? guess oldguess)
	guess
	(sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 2.0))

(define (average x y)
  (/ (+ x y) 2))

(sqrt 9)
