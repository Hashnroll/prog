;constructing vector
(define (make-vect x y)
  (cons x y))

;selectors of vector coordinats
(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

					;operations on vectors
