;; Simpson's Rule is a more accurate method of numerical
;; integration than the method illustrated above. Using
;; Simpson's Rule, the integral of a function f between
;; a and b is approximated as:

;; (h/3) (y(0) + 4y(1) + 2y(2) + 4y(3) + 2y(4) + ... + 2y(n-2) + 4y(n-1) + y(n)),

;; where h = (b - a)/n, for some even integer n, and y(k) = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson's Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000),
;; and compare the results to those of the integral procedure shown above.

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      (+ (term a)
         (sum term (next a) next b))))

(define (term x) (x))

(define (cube x)
  (* x (square x)))

(define (square x)
  (* x x))

