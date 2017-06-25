(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

;total-weight procedure
(define (total-weight mobile)
  (cond ((null? mobile) 0)
	((pair? mobile) (+ (total-weight (branch-structure (left-branch mobile)))
			   (total-weight (branch-structure (right-branch mobile)))))
	(else mobile)))

(define b-mobile (make-mobile (make-branch 1 2) (make-branch 1 2)))
(define c-mobile (make-mobile (make-branch 2 3) (make-branch 2 4)))
(define b-branch (make-branch 10 b-mobile))
(define c-branch (make-branch 10 c-mobile))
(define a-mobile (make-mobile b-branch b-branch))
(define a-branch (make-branch 10 a-mobile))
(define d-mobile (make-mobile a-branch c-branch))
(total-weight d-mobile)

;balanced?
(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))
(define (balanced? mobile)
  (cond ((pair? mobile)
	 (and (balanced? (branch-structure (left-branch mobile)))
	      (balanced? (branch-structure (right-branch mobile)))
	      (= (branch-torque (left-branch mobile))
		 (branch-torque (right-branch mobile)))))
	(else #t)))

(define d-mobile (make-mobile a-branch b-branch))
(balanced? d-mobile)

				     



