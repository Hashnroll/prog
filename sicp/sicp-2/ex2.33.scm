;ex2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (1+ y)) 0 sequence))

;ex2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff
		   (* x higher-terms)))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 2 3 0 5 0 2))

;ex2.35
(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))

(define (count-leaves t)
  (accumulate (lambda (x y)
		(+ (cond
			 ((not (pair? x)) 1)
			 (else (count-leaves x)))
		   y))
	      0
	      t))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (fringe t))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
			 (if (pair? node)
			     (count-leaves node)
			     1))
		       t)))
	      

(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))    
	    (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence) 
	  (accumulate op initial (cdr sequence)))))

;ex2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

;ex2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

(define mtr (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define vtr (list 1 2 3))
(matrix-*-vector mtr vtr)
  
(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n-cols m-row)) m)))

(define a (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define b (list (list 10 20 30) (list 40 50 60) (list 70 80 90)))
(matrix-*-matrix a b)

;ex2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ;3/2
(fold-left / 1 (list 1 2 3)) ;1/6

(fold-right list () (list 1 2 3))
(fold-left list () (list 1 2 3))

ex2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(reverse (list 1 2 3 4 5))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
	  (accumulate op init (cdr seq)))))

(reverse (list 1 2 3 4))


