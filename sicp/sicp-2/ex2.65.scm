(define x (make-tree 25 (make-tree 15 (make-tree 10 '() '()) (make-tree 20 '() '()) ) (make-tree 50 (make-tree 45 '() '()) (make-tree 55 () ()))))

(define y (make-tree 30 (make-tree 20 (make-tree 15 '() '()) (make-tree 25 '() '()) ) (make-tree 55 (make-tree 50 '() '()) (make-tree 60 () ()))))

;i rly dunno order of growth in the number of steps of these functions(so i'll try to leatn this theory and return to this task, cause i need O(n) at both)  
(define (union-set-tree tree1 tree2)
  (let ((list1 (tree->list tree1))
	(list2 (tree->list tree2)))
    (list->tree (union-set list1 list2))))

(define (intersection-set-tree tree1 tree2)
  (let ((list1 (tree->list tree1))
	(list2 (tree->list tree2)))
    (list->tree (intersection-set list1 list2))))

(tree->list x)
(tree->list y)
(tree->list (union-set-tree x y))
(tree->list (intersection-set-tree x y))
