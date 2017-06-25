(define one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)
(cadr one-through-four)
(cons 10 one-through-four)

;operations(list-ref, length, append)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 1)

(define (length items)
  (if (null? items)
      0
      (1+ (length (cdr items)))))
(define (length items)
  (define (length-iter res items)
    (if (null? items)
	res
	(length-iter (1+ res) (cdr items))))
  (length-iter 0 items))
 
(define odds (list 1 3 5 7))
(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares odds)






