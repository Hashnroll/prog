(define (scale-tree tree factor)
  (cond ((null? tree) ())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))
(scale-tree (list (list 1 2) (list 3 4)) 5)

;ex2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))
(define x (list (list 1 () 2) (list 3 4)))
(square-tree x)

;ex2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))
(define x (list 1 2 () 3 4))
(define (square-tree x) (tree-map square x))
	     
(map square x)

;ex2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x))
			  rest)))))
(subsets (list 1 2 3))
(map (lambda (x) 
       (append (list 1) x)) (subsets (list 2 3)))









