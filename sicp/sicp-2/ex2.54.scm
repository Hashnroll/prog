(define (equal? list1 list2)
  (cond ((and (pair? list1) (pair? list2))
	 (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
	((and (not (pair? list1)) (not (pair? list2)))
	 (eq? list1 list2))
	(else #f)))

(equal? '(this is a list) '(this is a list))
(car ''abracadabra)
(car (quote (quote abracadabra)))
			       
	   

	
	
      
	   
