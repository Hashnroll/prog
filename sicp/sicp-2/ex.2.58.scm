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
	(else
	 (error "unknown expression type -- DERIV" exp))))

;predicates
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

;helper for constructors
(define (=number? x y)
  (and (number? x)
       (= x y)))

;constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
	((=number? exp 1) base)
	((and (number? base) (number? exp)) (expt base exp))
	(else (list base '^ exp))))

;sum selectors
(define (addend s) (car s))

(define (augend s) (caddr s))

;product selectors
(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;exponentiation selectors
(define (base e) (car e))

(define (exponent e) (caddr e))

;b)

;predicates
(define (sum? expr)
  (memq '+ expr))

(define (product? expr)
  (and (not (memq '+ expr))
       (memq '* expr)))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

;selectors
(define (addend expr)
  (let ((end? (eq? '+ (car expr))))
    (cond (end? ())
	  (else (cons (car expr) (addend (cdr expr)))))))

(addend '((x + 3) * (y + 4) + 8))

(addend '(x + 2))

(define (augend expr)
  (let* ((second-operand (cdr (memq '+ expr))))
    (if (null? (cdr second-operand))
	(car second-operand)
	second-operand)))

(define (multiplier expr)
 (if (eq? '* (car expr))
      ()
      (cons (car expr)
	    (multiplier (cdr expr)))))

(define (multiplicand expr)
  (let* ((second-operand (cdr (memq '* expr))))
    (if (null? (cdr second-operand))
	(car second-operand)
	second-operand)))

(define l '(3 * x + y * 2 + z + d + t))
(aend l)

(define l '(3 + 2 * y))
(deriv l 'y)
(addend l)
(augend l)
(define m '(y * (z + 4) * (3 + x)))
(multiplier m)
(deriv m 'y)

	   
