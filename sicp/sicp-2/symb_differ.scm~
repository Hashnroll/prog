(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product (exponent exp)
			(make-exponentiation (base exp) (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;helper for constructors
(define (=number? x y)
  (and (number? x)
       (= x y)))

;constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
	((=number? exp 1) base)
	((and (number? base) (number? exp)) (expt base exp))
	(else (list '^ base exp))))

;sum check					     
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;sum selectors
(define (addend s) (cadr s))

(define (augend s) 
  (fold-right make-sum 0 (cddr s)))

;augend selector v2
(define (augend s)
  (let ((augend-element (cddr s)))
    (if (null? (cdr augend-element))
	(car augend-element)
	(cons '+ augend-element))))

;product check
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;product selectors
(define (multiplier p) (cadr p))

(define (multiplicand p)
  (fold-right make-product 1 (cddr p))) 
 
;multiplicand selector v2
(define (multiplicand p)
  (let ((multiplicand-element (cddr p)))
    (if (null? (cdr multiplicand-element))
	(car multiplicand-element)
	(cons '+ multiplicand-element))))

;exponentiation check
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))



					;exponentiation selectors
(define (base e) (cadr e))

(define (exponent e) (caddr e))

;test
(deriv '(+ (* x x) 1) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(^ x n) 'x)
(define l '(* x y z d f))
(deriv '(* (* x x) y z) 'x)
(deriv '(+ x y z (* d d) f) 'x)
