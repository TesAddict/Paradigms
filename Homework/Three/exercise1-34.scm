;; Suppose we define a procedure:

(define (f g) (g 2))

(define (square x) (* x x))

(f (lambda (z) (* z (+ z 1))))

;; (f f) -> (f 2) -> (2 2)