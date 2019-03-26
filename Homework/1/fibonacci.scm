(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;; (fib 5)
;; (fib 4)(fib 3)
;; (fib 3)(fib 2)(fib 2)(fib 1)
;; (fib 2)(fib 1)(fib 1)(fib 0)(fib 1)(fib 0)+1
;; (fib 1)(fib 0)+1+1+0+1+0+1
;; 1+0+1+1+0+1+0+1 = 5

(define (fib-iter count)
  (define (fib-iter-helper current last count)
    (cond ((= count 1) current)
          (else (fib-iter-helper (+ current last) current (- count 1)))))
  (fib-iter-helper 1 0 count))

;; (fib 5)
;; (fib-iter-helper 1 0 5)
;; (fib-iter-helper 1 1 4)
;; (fib-iter-helper 2 1 3)
;; (fib-iter-helper 3 2 2)
;; (fib-iter-helper 5 3 1)
;; 5
  

