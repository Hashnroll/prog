(define rand
  (let ((x (random 10000000)))
    (lambda ()
      (set! x (random 10000000))
      x)))

(estimate-pi 10000)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;exercises
;;ex3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define rectangle-area 
    (* (- x2 x1) (- y2 y1)))
  (* rectangle-area (monte-carlo trials (p x1 x2 y1 y2))))

(define (circle-predicate xc yc r)
  (lambda (xl xh yl yh)
    (lambda ()
      (let ((x (random-in-range xl xh))
	    (y (random-in-range yl yh)))
	(<= (+ (square (- x xc))
	       (square (- y yc)))
	    (square r))))))

(define (estimate-pi trials)
  (define unit-circle (circle-predicate 1 1 1))
  (estimate-integral unit-circle 0.0 2.0 0.0 2.0 trials))

(estimate-pi 100000)

;;ex3.6
(define rand-update 1+)
(define random-init 0)
(define rand
  (let ((x random-init))
    (define (dispatch msg)
      (cond ((eq? msg 'generate)
	     (begin (set! x (rand-update x))
		    x))
	    ((eq? msg 'reset)
	     (lambda (new-value)
	       (begin (set! x new-value)
		      new-value)))))
    dispatch))

(rand 'generate)
((rand 'reset) 2)
	     
	    


