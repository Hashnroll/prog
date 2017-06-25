(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
		(make-interval (- (upper-bound y))
			       (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (interval spans 0)" y)
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
(define x (make-interval 3 6)) 
(define y (make-interval -1 4))
(div-interval x y)

;ex2.12
(define (make-center-percent c p)
  (let ((width (abs (* c (/ p 100)))))
    (make-interval (- c width)
		   (+ c width)))))
(define (center x)
  (/ (+ (upper-bound x) (lower-bound x)) 2))
(define (percent x)
  (let ((l (lower-bound x))
	(u (upper-bound x)))
    (* 100 (/ (- u l) (+ u l)))))

(define x (make-center-percent 10 0.3))
(define y (make-center-percent 10 0.2))
(percent (mul-interval x y))
(lower-bound x)
(upper-bound x)
(center x)
(percent x)

;ex2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))
(define x (make-interval -2 2))
(define y (make-center-percent 100 0.01))

(mul-interval x x)






