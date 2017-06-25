(define x (cons (list 1 2) (list 3 4)))
(length x)
(length (list x x))

;
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(count-leaves (list x x))

(define huinya (list 1 (list 2 (list 3 4))))
(count-leaves huinya)





