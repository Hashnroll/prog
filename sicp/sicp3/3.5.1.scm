(define (stream-ref s n) 
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (1+ low) high))))
;;(cons 10000
;;      (delay (stream-enumerate-interval 10001 1000000)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

;;implementing delay and force
(define delay 
  (lambda ()))
(define (force delayed-object)
  (delayed-object))

;;memoized optimization
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))
(define delay
  (memo-proc (lambda ())))

;;exercises
;;ex3.50
(define (show x)
  (display-line x)
  x)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(apply square (list 2))
(define x (stream-map show (stream-enumerate-interval 0 10)))


(stream-ref x 5)
(stream-ref x 7)

;;3.52
(define sum 0) sum
(define (accum x)
  (set! sum (+ x sum))
  sum) sum
(define seq (stream-map accum (stream-enumerate-interval 1 20))) sum 
(define y (stream-filter even? seq)) sum
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq)) sum
(stream-ref y 7)
(display-stream z) sum

