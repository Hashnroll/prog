(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;;ex4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (1+ low) high)))

;;ex4.41
;;helpers
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low
	    (enumerate-interval (1+ low) high))))
(define (flatmap proc l)
  (fold-right append () (map proc l)))


(define (permutations l)
  (if (not (null? l))
      (flatmap (lambda (e)
		 (map (lambda (seq) (cons e seq)) (permutations (remove e l))))
	       l)
      (list ())))

(define (remove e l)
  (filter (lambda (x) (not (equal? e x))) l))

(define (multiple-dwelling-pred x)
  (let ((baker (car x))
	(cooper (cadr x))
	(fletcher (caddr x))
	(miller (cadddr x))
	(smith (car (cddddr x))))
    (and (not (= baker 5))
	 (not (= cooper 1))
	 (not (= fletcher 5))
	 (not (= fletcher 1))
	 (> miller cooper)
	 (not (= (abs (- smith fletcher)) 1))
	 (not (= (abs (- cooper fletcher)) 1)))))

(define (multiple-dwelling)
  (flatmap (lambda (x) (list (list 'baker (car x)) (list 'cooper (cadr x))
			 (list 'fletcher (caddr x)) (list 'miller (cadddr x))
			 (list 'smith (car (cddddr x)))))
       (filter multiple-dwelling-pred (permutations (list 1 2 3 4 5)))))

(multiple-dwelling)



