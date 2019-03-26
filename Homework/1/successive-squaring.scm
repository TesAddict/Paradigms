;; b^n = (b^(n/2))^2   if n is even
;; b^n = b * b^(n-1)   if n is odd

(define (my-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square(my-expt b (/ n 2))))
        (else (* b (my-expt b (- n 1))))))

(define (square n) (* n n))