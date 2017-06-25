(define (fi)
  (/ (+ 1 (sqrt 5)) 2))

(define (pow x y)
  (pow-iter 1 x y))

(define (pow-iter res x y)
  (if (= y 0)
      res
      (pow-iter (* res x) x (- y 1))))

(define (fib n)
 (/ (pow (fi) n) (sqrt 5)))

(fib 6)

(pow 2 10)













