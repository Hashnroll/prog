((lambda (x y z) (+ x y (square z))) 1 2 3)

;f(x, y) = x(1+x*y)^2+y(1-y)+(1+x*y)(1-y)
a=1+x*y, b=1-y => f(x, y) = x*a^2+y*b+a*b

;
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

;with lambda
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
	(* y b)
	(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;with let
(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(f 5 6)




