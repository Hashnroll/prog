(define (sqrt x)
  (define (sqrt-iter guess)
    (let ((next (improve-guess x guess)))
      (if (good-enough? next guess)
	  next
	  (sqrt-iter next))))
  (sqrt-iter 1.0))

(define (improve-guess x guess)
  (/ (+ guess (/ x guess)) 2))

(define (good-enough? next guess)
  (< (abs (- next guess)) (* guess 0.001)))

(sqrt 9)
