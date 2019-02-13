;; Newton's method for cube roots is based on the fact
;; that if y is an approximation to the cube root of x,
;; then a better approximation is given by the value:
;;
;;                (x/y^2 + 2y) / 3
;; Use this formula to implement a cube-root procedure
;; analogous to the square-root procedure.

(define (cube-root x)
  (define (good-enough? guess)
    (and (<= 0 (- guess (refine guess))) (>= 0.0000000000001 (- guess (refine guess)))))
  (define (refine guess)
    (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))
  (define (cube-root-iter guess)
    (cond ((good-enough? guess) guess)
          (else (cube-root-iter (refine guess)))))
  (cube-root-iter 1.0))

(define (square x) (* x x))
    



