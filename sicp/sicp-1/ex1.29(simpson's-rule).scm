;basic prosedures
(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (round-to-next x)
  (+ x (remainder x 2)))

;linear recursion sum
(define (sum term k next n)
  (if (> k n)
      0
      (+ (term k)
	 (sum term (next k) next n))))

;iteration sum
(define (itersum term k next n)
  (define (iter k result)
    (if (> k n)
	result
	(iter (next k) (+ result (term k)))))
  (iter k 0))
	   
;main idea
(define (simpson-rule f a b n)
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k fixed-n)) 1)
	     ((even? k) 2)
	     (else 4))
       (yk k)))
  (define (yk k)
    (f (+ a (* k h))))
  (define fixed-n (round-to-next n))
  (define h
    (/ (- b a) fixed-n))
  (* (/ h 3.0) (itersum simpson-term 0 inc fixed-n)))

(simpson-rule cube 0 1 100)

;ex1.30 added