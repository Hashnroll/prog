(define (cube-root x)
  (define (good-enough? guess oldguess)
    (<= (abs (- guess oldguess)) (* guess 0.00001)))
  (define (improve guess)
    (/ (+ (/ x (square guess))
	  (* 2 guess))
       3))
  (define (cube-root-iter guess oldguess)
    (display oldguess)
    (newline)
    (if (good-enough? guess oldguess)
	guess
	(cube-root-iter (improve guess) guess)))
  (if (< x 0)
      (* -1 (cube-root (abs x)))
      (cube-root-iter 1.0 2.0)))

(cube-root 27)



	   
      