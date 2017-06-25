(define (apply-generic op . args)  ;non-completed
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (let ((coerced-args
		 (map (lambda (main-arg)
			(map (lambda (coerced-arg)
			       (let* ((main-type (type-tag main-arg))
				      (coerced-t->main-t) (get-coersion main-type (type coerced-arg))))
			       (coerced-t->main-t coerced-arg))
			     args))
		      args)))
	    (if (> (length coerced-args) 0)
		(apply-generic op (car coerced-args))
		(apply-generic op (cdr coerced-args))
			       (else
				(error "No method for these types"
				       (list op type-tags))))))))))



