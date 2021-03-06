(define (raise x) (apply-generic 'raise x))

;;to integer package
(define (raise-int x) (make-rational x 1))
(put 'raise '(integer) raise-int)

;;to rational package
(define (raise-rat x) (make-real (/ (numer x) (denom x))))
(put 'raise '(rational) raise-rat)

;;to real-package
(define (raise-real x) (make-complex-from-real-imag x 0))
(put 'raise '(real) raise-real)

;;ex2.84(here is used raise spec from generic_operations_system.scm, not above
(define (apply-generic op . args)
  (define (raise-to arg type)
    (if (eq? (type-tag arg) type)
	arg
	(if (not (null? (get 'raise (list (type-tag arg)))))
	    (raise-to (raise arg) type)
	    ())))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for these types")
		    (let ((raised-arg1 (raise-to a1 type2))
			  (raised-arg2 (raise-to a2 type1)))
		      (cond ((not (null? raised-arg1))
			     (apply-generic op raised-arg1 a2))
			    ((not (null? raised-arg2))
			     (apply-generic op a1 raised-arg2))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))))
	      (error "No method for these types"
		     (list op type-tags)))))))


;;ex2.85 missed

;;ex2.86








(add 2 (make-complex-from-real-imag 2 3))






