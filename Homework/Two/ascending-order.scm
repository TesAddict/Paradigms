; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

(define (ascending-order integer)
  (cond ((= (floor (/ integer 10)) 0) 1)
        ((<= (floor (/ (remainder integer 100) 10)) (remainder integer 10)) (ascending-order (floor (/ integer 10))))
        (else 0)))


(define (ascending-order-recursive integer)
  (cond ((= (floor (/ integer 10)) 0) 1)
        (