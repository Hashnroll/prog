
(define x (make-tree 4 (make-tree 2 (make-tree 1 () ()) (make-tree 3 () ())) (make-tree 5 () ())))

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) #f)
	((= given-key (entry tree-of-records)) (entry tree-of-records))
	((< given-key (entry tree-of-records)) (lookup given-key (left-branch tree-of-records)))
	(else (lookup given-key (right-branch tree-of-records)))))

(lookup 0 x)
	 
	 
