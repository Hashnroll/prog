(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (else
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
	  (error "DELETE! called with an empty queue" queue))
	 (else (set-front-ptr! queue (cdr (front-ptr queue)))
	       queue)))
(define z (make-queue))
(insert-queue! z 'a)
(delete-queue! z)

;;exercises
;;ex3.21
(define (print-queue queue)
  (front-ptr queue))

;;ex3.22 not completed
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define empty-queue? (null? front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) (empty-
	     ((eq? m 'set-rear-ptr!) (lambda (item)
				      (set! rear-ptr item)))
	    ((eq? m 'empty-queue?) (null? front-ptr))
	    ((eq? m 'front-queue) (car front-ptr))
	    
	    

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))

(define (set-front-ptr! queue item) ((queue 'set-front-ptr) item))
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr) item))

(define (empty-queue? queue) (queue 'empty-queue?))

(define (front-queue queue) (queue 'front-queue))

;;ex3.23
(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (and (null? (front-ptr deque))
				  (null? (rear-ptr deque))))

(define prev cadr)
(define next cddr)
(define (set-next! elem v)
  (set-cdr! (cdr elem) v))
(define (set-prev! elem v)
  (set-car! (cdr elem) v))
(define value car)

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item
			(cons '() (front-ptr deque)))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-prev! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)))))

(define (front-insert-deque! deque item)
  (let ((new-pair (list '() item (front-ptr deque))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-prev! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)))))(define (front-insert-deque! deque item)
  (let ((new-pair (list '() item (front-ptr deque))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-prev! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item
			(cons (rear-ptr deque) '()))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-next! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	  (error "DELETE! called with an empty deque" deque))
	(else (set-front-ptr! deque (next (front-ptr deque)))
	      (if (not (null? (front-ptr deque)))
		  (set-prev! (front-ptr deque) '())
		  (set-rear-ptr! deque '())))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	  (error "DELETE! called with an empty deque" deque))
	(else (set-rear-ptr! deque (prev (rear-ptr deque)))
	      (if (not (null? (rear-ptr deque)))
		  (set-next! (rear-ptr deque) '())
		  (set-front-ptr! deque '())))))

(define (print-deque deque)
  (define (print-succ front-ptr)
    (cons (value front-ptr)
	  (if (not (null? (next front-ptr)))
	      (print-succ (next front-ptr))
	      '())))
  (if (null? (front-ptr deque))
      '()
      (print-succ (front-ptr deque))))


(define q (make-deque))

(print-deque q)

(rear-insert-deque! q 'n)
(rear-delete-deque! q)

(front-insert-deque! q 'c)
(front-delete-deque! q)

;;list representation of deque
(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (and (null? (front-ptr deque))
				  (null? (rear-ptr deque))))

(define prev car)
(define next caddr)
(define (set-next! elem v)
  (set-car! (cddr elem) v))
(define (set-prev! elem v)
  (set-car! elem v))
(define value cadr)

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (list '() item (front-ptr deque))))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-prev! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (list (rear-ptr deque) item '())))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair))
	  (else
	   (set-next! (rear-ptr deque) new-pair)
	   (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	  (error "DELETE! called with an empty deque" deque))
	(else (set-front-ptr! deque (next (front-ptr deque)))
	      (if (not (null? (front-ptr deque)))
		  (set-prev! (front-ptr deque) '())
		  (set-rear-ptr! deque '())))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	  (error "DELETE! called with an empty deque" deque))
	(else (set-rear-ptr! deque (prev (rear-ptr deque)))
	      (if (not (null? (rear-ptr deque)))
		  (set-next! (rear-ptr deque) '())
		  (set-front-ptr! deque '())))))

(define (print-deque deque)
  (define (print-succ front-ptr)
    (cons (value front-ptr)
	  (if (not (null? (next front-ptr)))
	      (print-succ (next front-ptr))
	      '())))
  (if (null? (front-ptr deque))
      '()
      (print-succ (front-ptr deque))))

(define deq (make-deque))

(front-insert-deque! deq 'x)
(rear-insert-deque! deq 'c)
(print-deque deq)
(front-delete-deque! deq)
(rear-delete-deque! deq)
