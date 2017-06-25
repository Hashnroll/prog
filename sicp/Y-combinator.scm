(define Y
  (lambda (f)
    (f (lambda (x) ((Y f) x)))))

(define almost-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 0)
	  1
	  (* n (f (- n 1)))))))

(define factorial (Y almost-factorial))

(factorial 5)

;;above is not Y-combinator, but it's enough for what i wanted to understand(farther later)
