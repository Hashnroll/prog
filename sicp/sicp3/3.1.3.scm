(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* counter product)
	      (1+ counter))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
	(counter 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin (set! counter (1+ counter))
		 (set! product (* counter product))
   		 (iter))))
    (iter))) ;;mistake is above in order of the sets!

(factorial 3)

;;exercises
;;ex3.7
(define (make-account balance secret-password)
  (define call-the-cops
	"You're in big trouble, boy")
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-tries 0))
    (define (dispatch m password)
      (if (equal? password secret-password)
	  (begin (set! wrong-tries 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Unknown request -- MAKE-ACCOUNT"
				    m))))
	  (begin (set! wrong-tries (1+ wrong-tries))
		 (if (= wrong-tries 7)
		     (lambda (x) call-the-cops)
		     (lambda (x) "Incorrect password.")))))
    dispatch))

(define peter-acc (make-account 10 'open-sesame))
((peter-acc 'deposit 'open-sesame) 5)

(define (make-joint acc acc-password joint-password)
  (lambda (msg pass)
    (if (eq? pass joint-password)
	(acc msg acc-password)
	(acc 'foo 'bad-pass))))

(define paul-acc (make-joint peter-acc 'open-sesame 'rousebad))

((paul-acc 'withdraw 'rousebad) 10)

;;ex3.8
(define f
  (let ((called #f))
    (lambda (x)
      (if called
	  0
	  (begin
	    (set! called #t)
	    x)))))

(+ (f 0) (f 1))
			   