(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (add-terms (term-list p1)
			    (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
	     (list p1 p2))))

(define (install-polynomial-package)
  ;;internal procedures
  ;;representation of poly
  (define (make-poly variable term-list)
    (cons (variable term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;;representation of terms and term lists

  ;;
  (define (add-poly p1 p2) ...)

  (define (mul-poly p1 p2) ...)

  ;;interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms l1 l2)
  (cond ((empty-termlist? l1) l2)
	((empty-termlist? l2) l1)
	(else
	 (let ((t1 (first-term l1)) (t2 (first-term l2)))
	   (cond ((> (order t1) (order t2))
		  (adjoin-term
		   t1 (add-terms (rest-terms l1) l2)))
		 ((< (order t1) (order t2))
		  (adjoin-term
		   t2 (add-terms l1 (rest-terms l2))))
		 (else
		  (adjoin-term
		   (make-term (order t1)
			      (add (coeff t1) (coeff t2)))
		   (add-terms (rest-terms l1)
			      (rest-terms l2)))))))))

(define (mul-terms l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
		 (mul-term (rest-terms l1) l2))))
(define (mul-term-by-all-terms t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
	(adjoin-term
	 (make-term (+ (order t1) (order t2))
		    (mul (coeff t1) (coeff t2)))
	 (mul-term-by-all-terms t1 (rest-terms l))))))



     
		 
		 
   
		
