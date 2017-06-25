(define zero
  (lambda (g) (lambda (x) x)))
(define one
  (lambda (g) (lambda (x) (g x))))
(define two
  (lambda (g) (lambda (x) (g (g x)))))
(define start
  ((one 1+) 1))

((add-1 two) 1+)
      
(define (add-1 n) 
  (lambda (f) (f ((n f) start)))))

(define (+ m n)
  (lambda (f) ((m f) ((n f) start))))

((+ two one) times_2)

;
(define (times_2 x)
  (* 2 x))


