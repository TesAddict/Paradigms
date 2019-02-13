(define (fact-recur x)
  (cond ((= x 1) 1)
        (else (* x (fact-recur (- x 1))))))

(define (factorial-iter count product)
  (cond ((= count 1) product)
        ((factorial-iter (- count 1) (* count product)))))

(define (fact-iter x)
  (factorial-iter x 1))

;; Exercise 1.9
;; Each of the following two procedures defines a method
;; for adding two positive integer terms of the procedure
;; inc, which increments its argument by 1, and dec, which
;; decrements its argument by 1.

(define (plus1 a b)
  (if (= a 0) b (inc (plus1 (dec a) b))))

(define (plus2 a b)
  (if (= a 0) b (plus2 (dec a) (inc b))))

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

;; Using the substitution model, illustrate the process generated
;; by each procedure in evaluating (+ 4 5). Are these processes
;; iterative or recursive?

;; plus1 substitution model (Tail Recursive)
;; (plus1 4 5)
;; (inc (plus1 3 5))
;; (inc (inc (plus1 2 5)))
;; (inc (inc (inc (plus1 1 5))))
;; (inc (inc (inc (inc (5)))))
;; (inc (inc (inc (6))))
;; (inc (inc (7)))
;; (inc (8))
;; 9

;; plus2 substitution model (Iterative Process)
;; (plus2 4 5)
;; (plus2 3 6)
;; (plus2 2 7)
;; (plus2 1 8)
;; 9