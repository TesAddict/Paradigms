; Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.

(define (square x) (* x x))

(define (sum-of-squares a b) (+ (square a) (square b)))

(define (answer a b c)
  (cond ((> a b) (cond ((> b c) (sum-of-squares a b)) (else (sum-of-squares a c))))
        ((> b a) (cond ((> a c) (sum-of-squares b a)) (else (sum-of-squares b c))))
        ((> c a) (cond ((> a b) (sum-of-squares c a)) (else (sum-of-squares c b))))))
