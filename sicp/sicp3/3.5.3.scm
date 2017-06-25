(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

(display-stream (sqrt-stream 2))

;;
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(display-stream pi-stream)

;;euler transform
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))
(display-stream (euler-transform pi-stream))

;;tableau
(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(display-stream (accelerated-sequence euler-transform pi-stream))

;;exercises
;;ex3.64
(define (stream-limit s a)
  (let ((first (stream-car s))
	(second (stream-car (stream-cdr s))))
    (if (< (abs (- first second)) a)
	second
	(stream-limit (stream-cdr s) a))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.001)

(define integers
  (cons-stream 1 (stream-map (lambda (x)
			       (1+ x))
			     integers)))
(display-stream integers)

;;infinite streams of pairs

;;recalling
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low
	    (enumerate-interval (1+ low) high))))

(define (flatmap op list)
  (fold-right append () (map op list)))

(define (divides? k i)
  (= (remainder k i) 0))

(define (prime? k)
  (define (iter i)
    (cond ((> (square i) k)
	   #t)
	  ((divides? k i)
	   #f)
	  (else (iter (1+ i)))))
  (iter 2))

(define (prime-sum-pairs n)
  (filter (lambda (l)
	    (prime? (+ (car l) (cadr l))))
	  (flatmap (lambda (i)
		     (map (lambda (j)
			    (list i j))
			  (enumerate-interval 1 (-1+ i))))
		   (enumerate-interval 1 n))))

(prime-sum-pairs 10)

;;
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(display-stream (pairs integers integers))


;;ex3.68
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

(display-stream (pairs integers integers))

;;ex3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
		(stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr s) (stream-cdr s)))))

(define pythagorean-triples
  (stream-filter (lambda (t) (= (+ (square (car t)) (square (cadr t))) (square (caddr t)))) (triples integers integers integers)))

;;ex3.70
;;a
(define (sum pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((<= (weight s1car)
		     (weight s2car))
		  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
		 ((> (weight s1car)
		     (weight s2car))
		  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))
	   
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define l1
  (cons-stream 5 integers))
(define l2
  (cons-stream 5 integers))

(define a (weighted-pairs integers integers sum))

;;b
(define s (cons-stream 1 (merge (merge (scale-stream s 2) (scale-stream s 3)) (scale-stream s 5))))

(define (sum pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define b (weighted-pairs s s sum))

(display-stream b)

;;ex3.71
(define (cube x)
  (* x x x))
(define sum-cubes
  (lambda (x) (+ (cube (car x)) (cube (cadr x)))))

(define (filter x f)
  (if (= (f (stream-car x))
	 (f (stream-car (stream-cdr x))))
      (cons-stream (stream-car x)
		   (filter (stream-cdr (stream-cdr x)) f))
      (filter (stream-cdr x) f)))

(define ramanujan
  (stream-map (lambda (x) (sum-cubes x)) (filter (weighted-pairs integers integers sum-cubes) sum-cubes)))

(display-stream ramanujan)
      
;;ex3.72
(define sum-of-squares
  (lambda (x) (+ (square (car x)) (square (cadr x)))))

(define (filter x f)
  (if (= (f (stream-car x))
	 (f (stream-car (stream-cdr x)))
	 (f ((stream-car (stream-cdr (stream-cdr x))))))
      (cons-stream (stream-car x)
		   (filter (stream-cdr (stream-cdr (stream-cdr x)) f)))
      (filter (stream-cdr x) f)))

(define sum-of-squares-order-stream
  (weighted-pairs integers integers sum-of-squares))

(define (a f x)
  (let* ((scar (stream-car x))
	(scadr (stream-car (stream-cdr x)))
	(scaddr (stream-car (stream-cdr (stream-cdr x))))
	(value (f scar)))
    (if (= value
	   (f scadr)
	   (f scaddr))
	(cons-stream (list value scar scadr scaddr)
		     (a f (stream-cdr (stream-cdr (stream-cdr x)))))
	(a f (stream-cdr x)))))

(display-stream (a sum-of-squares sum-of-squares-order-stream))

;;
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)

;;ex3.75
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
		 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream)
				      avpt))))

;;ex3.76
(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (smooth s)
  (stream-map (lambda (x1 x2) (average x1 x2))
	      (cons 0 s)
	      s))
  
(define (make-zero-crossings input-stream)
  (let ((smoothed-stream (smooth input-stream))))
    (cons-stream (sign-change-detector (stream-car smoothed-stream)
				       (stream-car (stream-cdr smoothed-stream)))
		 (make-zero-crossings (stream-cdr input-stream))))


;;ex3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt))))
				      

			      
		
