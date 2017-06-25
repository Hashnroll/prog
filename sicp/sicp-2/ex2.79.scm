(define (equ? x y) (apply-generic 'equ? x y))

;scheme-number
(put 'equ? '(scheme-number scheme-number) =)

;rational
(define (equ? x y)
  (= (* (numer x) (denom y)) (* (denom x) (numer y))))
(put 'equ? '(rational rational) equ?)

;complex
(define (equ? x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ?)

(define (equ? x y) (apply-generic 'equ? x y))
     
