(driver-loop)

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
	((null? list2) list1)
	(else (cons (+ (car list1) (car list2))
		    (add-lists (cdr list1) (cdr list2))))))

;;
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

(list-ref integers 10)

;;ex4.34 not completed
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(cons 1 2)

