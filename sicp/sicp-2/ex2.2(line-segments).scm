;;printing
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;SEGMENT - 2nd abstraction
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

;;POINT - 1st abstraction
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

;;example
(define a (make-point 1 2))
(define b (make-point 3 4))
(define line (make-segment a b))

;;midpoint
(define (midpoint-segment l)
  (define (average a b) (/ (+ a b) 2))
  (make-point (average (+ (x-point (start-segment l))) (x-point (end-segment l)))
	      (average (+ (y-point (start-segment l))) (y-point (end-segment l)))))

;;example
(define o (midpoint-segment line))
(print-point o)

;ex2.3
;;1st representation
(define (make-rectangle p1 p2)
  (let ((x1 (x-point p1))
	(y1 (y-point p1))
	(x2 (x-point p2))
	(y2 (y-point p2)))
     (cond ((and (< x1 x2) (< y1 y2)) (make-segment p1 p2))
	   ((and (< x2 x1) (< y2 y1)) (make-segment p2 p1))
	   ((and (< x1 x2) (< y2 y1)) (make-segment (make-point x1 y2) (make-point x2 y1)))
	   ((else (make-segment (make-point x1 y2) (make-point x2 y1)))))))
(define (bottom-left r)
  (car r))
(define (top-right r)
  (cdr r))

;;2nd representation
(define (make-rectangle p1 p2)
  (let ((x1 (x-point p1))
	(y1 (y-point p1))
	(x2 (x-point p2))
	(y2 (y-point p2)))
    (let ((width (abs (- x1 x2)))
	  (height (abs (- y1 y2))))
      (cond ((and (< x1 x2) (< y1 y2)) (cons p1 (cons width height)))
	   ((and (< x2 x1) (< y2 y1)) (cons p2 (cons width height)))
	   ((and (< x1 x2) (< y2 y1)) (cons (make-point x1 y2) (cons width height)))
	   ((else (cons (make-point x1 y2) (width height))))))))

(define (bottom-left r)
  (car r))
(define (top-right r)
  (make-point (+ (x-point (bottom-left r)) (car (cdr r)))
	      (+ (y-point (bottom-left r)) (cdr (cdr r)))))

;;common width and height for above 2 representations
(define (width r)
  (- (x-point (top-right r)) (x-point (bottom-left r))))
(define (height r)
  (- (y-point (top-right r)) (y-point (bottom-left r))))

;;3rd representation(rectangle can be rotated)
(define (make-rectangle p1 p2 height)
  (cons (make-segment p1 p2)
	(let ((x1 (x-point p1))
	      (x2 (x-point p2))
	      (y1 (y-point p1))
	      (y2 (y-point p2)))
	  (let ((theta (atan (/ (- y2 y1)
			       (- x2 x1)))))
	    (make-segment p1 (make-point (- x1 (* height (sin theta))) (+ y1 (* height (cos theta)))))))))

(define (width r)
  (length (base r)))
(define (base r)
  (car r))
(define (length side)
  (sqrt (+ (square (- (y-point (end-segment side))
		      (y-point (start-segment side))))
	   (square (- (x-point (end-segment side))
		      (x-point (start-segment side)))))))
(define (height r)
  (length (perp r)))
(define (perp r)
  (cdr r))
(print-point (end-segment (perp r1)))

;;independent(of 3 representations) perimeter and area
(define (perimeter r)
  (+ (* 2 (width r)) 
     (* 2 (height r))))
		
(define (area r)
  (* (width r) (height r)))
 
;example
(define a (make-point 0 0))
(define b (make-point 3 4))
(define r1 (make-rectangle a b 5))
(perimeter r1)
(area r1)








