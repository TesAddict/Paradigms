;; The good-enough? test used in computing square roots
;; will not be very effective for finding the square roots
;; of very small numbers. Also, in real computers, arithmetic
;; operations are almost always performed with limited precision.
;; This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails
;; for small and large numbers. An alternative strategy for
;; implementing good-enough? is to watch how guess changes
;; from one iteration to the next and to stop when the change is
;; a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work
;; better for small and large numbers?

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; The procedure has been modified to go to the machines
;; precision. Eventually, the computer will not be able
;; to improve the guess anymore, and comparing the current
;; guess to the better guess will yield an answer of 0.
;; At this point, we know we have the highest level of precision
;; possible, and can safely return the value true.
(define (good-enough? guess x)
  (= guess (improve guess x)))

(define (sqrt-alt x)
  (sqrt-iter 1.0 x))