;;ex3.12
;;constructor
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;;mutator
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(define w (append! x y))
w
(cdr x)

;;ex3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(last-pair z)

;;ex3.14
(define (reverse! x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (reverse! v))


;;sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;;ex3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(count-pairs (list 1 2 3)) ;;3

(define x (list 'a 'b))
(define y (cons x (cdr x)))
(count-pairs y) ;;4

(define x (list 'b))
(define y (cons x x))
(define z (cons y y))
(count-pairs z) ;;7

(define x (list 'a))
(define y (list 'b))
(set-car! x y)
(set-car! y x)
(define z (cons x x))
(count-pairs z) ;;never returns

;;ex3.17
(define (count-pairs x)
  (let ((counted-lists ()))
    (define (iter x)
      (if (not (pair? x))
	  0
	  (+ (iter (car x))
	     (iter (cdr x))
	     (if (memq x counted-lists)
		 0
		 (begin (set! counted-lists (cons x counted-lists))
			1)))))
    (iter x))) ;;goes into infinite loops

(count-pairs (list 1 2 3)) ;;3

(define x (list 'a 'b))
(define y (cons x (cdr x)))
(count-pairs y) ;;4

(define x (list 'b))
(define y (cons x x))
(define z (cons y y))
(count-pairs z) ;;7

(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
	  0
	  (begin
	    (set! encountered (cons x encountered))
	    (+ (helper (car x))
	       (helper (cdr x))
	       1))))
    (helper x))) ;;doesn't go into infinite loops

;;ex3.18
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x (list 1 2 3))
(define z (make-cycle x))

(define (cycle? x)
  (define visited ())
  (define (iter x)
    (set! visited (cons x visited))
    (cond ((null? (cdr x)) false)
	  ((memq (cdr x) visited) true)
	  (else (iter (cdr x)))))
  (iter x)) ;;not general solution(only for lists, not list structures)

(define y (cons 1 (make-cycle (list 2 3 4))))
(cycle? y) ;;#t - success

(define x (make-cycle (list 1 2 3)))
(define y (list x x))
(cycle? y) ;;#f - wrong answer if we percieve above solution as general for list structures

(define (cycle? x)
  (define (iter x visited)
    (cond ((not (pair? x)) #f)
	  ((memq x visited) #t)
	  (else (or (iter (car x) (cons x visited))
		    (iter (cdr x) (cons x visited))))))
  (iter x '())) ;;general solution for list structures

(define x (make-cycle (list 'a 'b 'c)))

(define y (list x x))
(cycle? y) ;;#t - success

(define z (reverse! x))
z
		
(define (cycle? x)
  (define (iter x cont elem num)
    (cond ((null? (cdr x)) false)
	  ((eq? x elem)
	   (display (list (car x) (car elem)))
	   true)
	  (else (if (= cont num)
		    (iter (cdr x) 0 x (1+ num))
		    (iter (cdr x) (1+ cont) elem num)))))
  (iter x 0 () 0))

(cycle? (cons 1 (make-cycle (list 1 2 3))))

(define x (make-cycle (list 1 2 3)))
(eq? (cons 2 (cons 3 x)) x)

;;mutation and assignment
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
	  ((eq? m 'cdr) y)
	  ((eq? m 'set-car!) set-x!)
	  ((eq? m 'set-cdr!) set-y!)
	  (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

