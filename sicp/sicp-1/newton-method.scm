;
(define (cube x)
  (* x x x))

(define dx 0.00001)

;
(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

;
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) (abs (* 0.001 v2))))
  (define (try guess)
    (let ((next (f guess)))
      (print guess next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

vv(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))
		 1312.514))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    newton-transform
			    1.0))

(define (4-root x)
  (fixed-point-of-transform (lambda (y) (- (fast-expt-iter y 4) x))
			    (newton-transform)
			    1.0))
;ex1.40
(define (cube x)
  (* x x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
(define (zeros-cubic a b c)
  (newton-method (cubic a b c) 1.0))

(zeros-cubic -2 1 1)

;ex1.41
(define (inc x)
  (+ x 1))
(define (double f) 
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)

;ex1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;ex1.43(rec)

(define (repeated f n)
    (if (= n 0)
	(lambda (x) x)
	(compose f (repeated f (- n 1))))))

;ex1.43(iter)

(define (repeated f n)
  (define (iter res n)
    (if (= n 0)
	res
	(iter (compose f res) (- n 1))))
  (iter (lambda (x) x) n))
	
((repeated square 2) 5)

ex1.44(smoothed function)

(define (smooth f)
  (lambda (x) (/ 
	       (+ 
		(f (- x dx)) 
		(f x) 
		(f (+ x dx))) 
	       3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))
((n-fold-smooth cos 10) 1.0)
;ex1.45
(define (n-root n x)
  (fixed-point-of-transform (lambda (y) (/ x (pow y (- n 1))))
			    (repeated average-damp (floor (/ (log n) (log 2))))
			    1.0))

(define (n-root n x)
  (fixed-point-of-transform (lambda (y) (- (pow y n) x))
			    newton-transform
			    1.0))
(n-root 15 (pow 2 15))

;ex1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (let ((next (improve guess)))
      (if (close-enough? guess next)
	  next
	  ((iterative-improve good-enough? improve) next)))))

(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))

;fixed-point via iterative-improve
(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)

;sqrt via iterative-improve
(define (sqrt x)
  ((iterative-improve close-enough? (average-damp (lambda (y) (/ x y)))) 1.0))

(sqrt 9)





