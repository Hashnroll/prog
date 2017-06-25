(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))

(define b (list (list 7)))
(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;ex2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(cons y x)

;ex2.27
(define (deep-reverse a)
  (if (null? a)
      a
      (append (deep-reverse (cdr a))
	      (list (if (pair? (car a))
			(deep-reverse (car a))
			(car a))))))
(define (deep-reverse a)
  (if (pair? a)
      (append (deep-reverse (cdr a))
	      (list (deep-reverse (car a))))
      a))
      
(define z (list 1 (list 2 (list 3 4)) 5))
z
(deep-reverse z)	   

;ex2.28
(define (fringe x)
  (cond ((null? x) ())
	((pair? x)
	 (append (fringe (car x))
		 (fringe (cdr x))))       
	(else (list x))))

(define x (list (list 1 2) (list 3 4)))
(fringe ())
(fringe 3)


      
