;; The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher order procedures. Write
;; an analogous procedure called product that returns the product of the
;; values of a function at points over a given range. Show how to define
;; factorial in terms of product. Also use product to compute approximations
;; to pie using the formula:
;;
;;       pie    2 * 4 * 4 * 6 * 6 * 8 ...
;;       --- = --------------------------
;;        4     3 * 3 * 5 * 5 * 7 * 7 ...

;; Iterative
(define (product a next b term answer)
  (cond ((> a b) answer)
        (else (product (next a) next b term (* (term a b) answer)))))

;; Recursive
(define (product-recur a next b term)
  (cond ((> a b) 1)
        (else (* (term a b) (product-recur (next a) next b term)))))

(define (next-num a) (+ a 2))

(define (next a) (+ a 1))

(define (square-special a b)
  (cond ((= a 2) a)
        ((= a 3) (* a a b))
        (else (* a a))))

(define (default a b) a)

;; Here is approx pie with provided equation. Iterative 
(define (approx-pie n)
  (* 4.0 (/ (product 2 next-num n square-special 1) (product 3 next-num n square-special 1))))

;; Here is approx pie with provided equation. Recursive
(define (approx-pie-recur n)
  (* 4.0 (/ (product-recur 2 next-num n square-special) (product-recur 3 next-num n square-special))))

;; Here is factorial 
(define (factorial n)
  (product 1 next n default 1))