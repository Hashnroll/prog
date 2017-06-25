(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) 
	 (sum term (next a) next b))))

(define (sum-of-cubes a b)
  (sum (lambda (x) (* x x x)) a (lambda (x) (+ x 1))  b))

(sum-of-cubes 1 3)

;
(define (sum-of-integers a b)
  (sum (lambda (x) x) 
       a 
       (lambda (x) (+ x 1)) 
       b))

(sum-of-integers 1 10)

;
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) 
       a 
       (lambda (x) (+ x 4))
       b))

(* 8 (pi-sum 1 1000))

;furthermore - using sum abstraction for computing define integral approximately
(define (integral f a b dx)
  (* (sum f 
	  (+ a (/ dx 2.0)) 
	  (lambda (x) (+ x dx))
	  b)
     dx))

(integral cube 0 1 0.01)







