(define (pascal-triangle i j)
  (cond ((or (< j 1) (> j i)) 0)
        ((or (= j 1) (= j i)) 1)
	(else (+ (pascal-triangle (- i 1) (- j 1))
		 (pascal-triangle (- i 1) j)))))

(pascal-triangle 12 6)
			       