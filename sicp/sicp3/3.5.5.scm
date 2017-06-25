(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x))) ;;assignment style

(define random-init (1+ (random 1000000)))
(define random-numbers
  (cons-stream random-init
	       (stream-map (lambda (x) (1+ (random 1000000))) random-numbers))) ;;stream style
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
			random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
	      (monte-carlo cesaro-stream 0 0)))

(display-stream pi)

;;ex3.81
(define rand-update
  (lambda (x) (1+ (random 1000000))))

(define (random-generator input-stream)
  (define (next random-init stream)
    (let ((mes (stream-car stream)))
      (cond ((not (pair? mes))
	     (if (equal? mes 'generate)
		 (cons-stream (rand-update random-init)
			      (next (rand-update random-init) (stream-cdr stream)))
		 (error "Unknown msg")))
	    (else
	     (if (equal? (car mes) 'reset)
		 (cons-stream (cdr mes)
			      (next (cdr mes) (stream-cdr stream)))
		 (error "Unknown msg"))))))
  (next random-init input-stream))

(define gen-stream
  (cons-stream (cons 'reset 10)
	       (cons-stream 'generate
			    (cons-stream (cons 'reset 50)
					 (cons-stream 'generate
						      gen-stream)))))

(stream-car (stream-cdr (stream-cdr (random-generator gen-stream))))

;;ex3.82
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;generic integral predicate
(define (under-curve? pred)
  (define next
    (lambda (xl xh yl yh)
      (let ((x (random-in-range xl xh))
	    (y (random-in-range yl yh)))
	(cons-stream (pred x y)
		     (next xl xh yl yh)))))
  next)

;;
(define (circle-stream r x0 y0)
  (define (inside-circle? x y)
    (<= (+ (square (- x x0)) (square (- y y0))) (square r)))
  (under-curve? inside-circle?))

(define (estimate-integral p x1 x2 y1 y2)
  (let ((S (* (- x2 x1) (- y2 y1))))
    (stream-map (lambda (p) (* S p))
		(monte-carlo (p x1 x2 y1 y2) 0 0))))

(define pi
  (stream-map (lambda (x) (* 4 x)) (estimate-integral (circle-stream 0.5 0.5 0.5) 0.0 1 0.0 1)))

;;
(define (sin-stream)
  (under-curve? (lambda (x y) (<= (sin x) y))))

(define one
  (estimate-integral (sin-stream) 0.0 3.1415 0.0 1.0))
