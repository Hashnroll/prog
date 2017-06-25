(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (1+ low) high))))

(accumulate append
	    ()
	    (map (lambda (i)
		   (map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 3)))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

;
(define (divides? a b)
  (= 0 (remainder a b)))
;
(define (prime? n)
  (define (iter k)
    (if (<= k (sqrt n))
	(if (divides? n k)
	    #f
	    (iter (1+ k)))
	#t))
  (iter 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))
(prime-sum-pairs 6)


;permutations
(define (perm s)
  (if (null? s)
      (list ())
      (flatmap 
       (lambda (x)
	 (map (lambda (p) (cons x p))
	      (perm (remove x s))))
       s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(perm (list 1 2 3))

;ex2.41(triples of positive i,j,k which <= n and sum to s
(define (triples n s)
  (filter (lambda (triple) (= (accumulate + 0 triple) s))
	  (unique-triples n)))

(define (unique-triples n)
  (flatmap 
   (lambda (i)
     (flatmap 
      (lambda (j) 
	    (map 
	     (lambda (k)
	       (list i j k))
	     (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(triples 10 20)











