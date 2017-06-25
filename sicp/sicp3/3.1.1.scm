(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 50)
(w2 40)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown reques -- MAKE-ACCOUNT"
		       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'deposit) 40)

;;exercises
;;3.1
(define (make-accumulator sum)
  (lambda (addend)
    (begin (set! sum (+ sum addend))
	   sum)))

(define a (make-accumulator 5))

;;ex3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((equal? arg 'how-many-calls?)
	     calls)
	    ((equal? arg 'reset-count)
	     (begin (set! calls 0)
		    "Count is succesfully reseted"))
	    (else (begin
		    (set! calls (1+ calls))
		    (f arg)))))))

(define s (make-monitored sqrt))
(s 'how-many-calls?)
(s 'reset-count)
(s 100)	   
  
;;ex3.3
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

(define acc (make-account 100 'seecret))
((acc 'withdraw 'secret) 50)


