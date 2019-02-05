(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-alt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6 -----

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-mod guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-mod (improve guess x)
                     x)))

; When you call sqrt-iter-mod with the new-if it gets stuck in an infinite
; loop. This is because scheme is set up to be applicative-order on this
; interpreter/machine. This means that all the inputs of new-if are
; computed before the procedure begins. One of the parameters, the else clause,
; recursively calls the sqrt-iter-mod, which is why the infinite loop happens. 