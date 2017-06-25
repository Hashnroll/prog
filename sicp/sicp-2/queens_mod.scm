;; Another representation of data in "QUEENS!" program.
;; One queen is represented by pair: row-col.
;; Board position is represented by list of row-col pairs (list (cons r1 c1) (cons r2 c2) ... (cons rn cn))
;; Each new queen is added to the beginning of representation

;; Constructor
(define (place-queen row col)
  (cons row col))

;; Selectors
(define (queen-row queen)
  (car queen))

(define (queen-col queen)
  (cdr queen))

;; Program itself

(define (adjoin-position row column board)
  (cons (place-queen row column)
	board))

(define empty-board ())


(define (same-row q1 q2)
  (= (queen-row q1) (queen-row q2)))
(define (same-diag q1 q2)
  (= (abs (- (queen-row q1)
	     (queen-row q2)))
     (abs (- (queen-col q1)
	     (queen-col q2)))))
;;

(define (safe-from-attack q1 q2)
  (and (not (same-row q1 q2))
       (not (same-diag q1 q2))))

(define (safe? col positions)
  (let ((first-q (car positions)))
    (accumulate (lambda (a b) (and a b))
		true
		(map (lambda (checked-q) 
		       (safe-from-attack first-q checked-q))
		     (cdr positions)))))




