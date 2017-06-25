;; Supporting functions(among others, not described here)
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (not (> low high))
      (cons low (enumerate-interval (1+ low) high))
      ()))

;; Program itself "QUEENS!"
;;
;; Representation of board is list in which 
;; index of element corresponds with the column of placing some queen
;; and element itself is the row of placing the queen.
;; For example,
;;     (list 1 1 1 1) represents board of four queens across the top row,
;;     (list 1 2 3 4) represents board of four queens on a diagonal
;;                    from left-top corner to the right-bottom corner,
;; and (list 2 4 1 3) is a solution to the 4 Queens problem.
;;
;;
;; A set of board positions is represented as a list of positions.
;; For example, (list (list 1 1 1 1) (list 1 2 3 4) (list 2 4 1 3))
;; is the set of position given above.

(define empty-board ())	    

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k positions)
  (let ((new (list-ref positions (- k 1))))  
    (define (check-position m)
      (if (< m 0)
	  #t
	  (let ((checked (list-ref positions m)))
	    (if (or (= new checked)
		    (= (abs (- (- k 1) m))
		       (abs (- new checked))))
		#f
		(check-position (- m 1))))))
    (if (= k 1)
	#t
      	(check-position (- k 2)))))

;; different safe?, it's slower than one above(i dunno why but maybe coz there're more calls of other functions,
;; and all these accumulates with maps most likely influence runtime)
(define (get-row positions column)
  (if (= column 1)
      (car positions)
      (get-row (cdr positions) (- column 1))))

(define (safe-row? k positions)
  (not (accumulate boolean/or false (same-row? k positions))))
(define (same-row? k positions)
  (let ((current-row (get-row positions k)))
    (map (lambda (index) 
	   (= current-row
	      (get-row positions index)))
	 (enumerate-interval 1 (- k 1)))))

(define (safe-diagonal? k positions)
  (not (accumulate boolean/or false (diagonal? k positions))))
(define (diagonal? k positions)
  (let ((current-row (get-row positions k)))
    (map (lambda (index)
	   (= (abs (- current-row (get-row positions index)))
	      (abs (- k index))))
	 (enumerate-interval 1 (- k 1)))))

(define (safe? k positions)
  (and (safe-row? k positions)
       (safe-diagonal? k positions)))

;;test, time test
(define (check-time f start)
    (if f
	(- (runtime) start)))

(queens 8)
(check-time (queens 8) (runtime))

;;ex2.43(interchaged nested mappings)
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(check-time (queens 7) (runtime))

(define (map proc seq)
  (if (null? seq)
      ()
      (cons (proc (car seq))
	    (map proc (cdr seq)))))

(sqrt (+ (* 4 (square (/ 0.5 25))) (square (/ 0.5 13))))







