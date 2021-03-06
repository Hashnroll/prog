(define (count-change amount)
    (define (changing amount kinds-of-coins)
      (cond ((= amount 0) 1)
	    ((or (< amount 0) (= kinds-of-coins 0)) 0)
	    (else (+
		(changing (- amount (first-coin)) kinds-of-coins)
		(changing amount (- kinds-of-coins 1)))))
      (define (first-coin)
	(cond ((= kinds-of-coin 1) 1)
	      ((= kinds-of-coin 2) 5)
	      ((= kinds-of-coin 3) 10)
	      ((= kinds-of-coin 4) 25)
	      ((= kinds-of-coin 5) 50)
	      (else 0))))
    (changing amount 5))

(count-change 100)

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+
	       (cc amount 
		   (- kinds-of-coins 1))
	       (cc (- amount (first-coin kinds-of-coins)) 
		   kinds-of-coins)))))

(define (first-coin kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 11)
