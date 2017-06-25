(define a 1)
(define b 2)

(list a b)    ;(1 2)
(list 'a 'b)  ;(a b)
(list 'a b)   ;(a 2)

(car '(a b))

					;memq
(define (memq item x)
  (cond ((null? x) #f)
	((eq? item (car x)) x)
	(else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))
