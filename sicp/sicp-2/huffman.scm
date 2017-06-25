;leaf constructor and selectors(and leaf? check)
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;tree constructor and selectors
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;all symbols of the tree to one list 
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

;find general tree weight
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((<= (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;exercises
;ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;ex2.68
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
	    (right (right-branch tree)))
	(cond ((element-of-set? symbol (symbols left))
	       (cons '0 (encode-symbol symbol left)))
	      ((element-of-set? symbol (symbols right))
	       (cons '1 (encode-symbol symbol right)))
	      (else (error "there is no such symbol in the tree"))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

;ex2.69	 
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define test-tree
  (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6))))

(encode '(A B C D) test-tree)

(define (successive-merge leaves) 
  (cond ((null? leaves) leaves)
	((null? (cdr leaves)) (car leaves))
	(else
	 (successive-merge
	  (adjoin-set (if (and (leaf? (car leaves)) (not (leaf? (cadr leaves))))
			  (make-code-tree (car leaves) (cadr leaves))
			  (make-code-tree (cadr leaves) (car leaves)))
		      (cddr leaves))))))

;ex2.70
(define alphabet '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define code-tree (generate-huffman-tree alphabet))
(length (encode '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip sha boom) code-tree))

;ex2.71
(define bin-alph '((a 1) (b 2) (c 4) (d 8)))

(define bin-code-tree (generate-huffman-tree bin-alph))
(encode '(a) bin-code-tree)
  

  
