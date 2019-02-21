;; A function f is defined by the rule that
;; f(n) = (n if n < 3) and (f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3)

;; Write a procedure that computes f by means of a recursive
;; process. Write a procedure that computes f by means of an
;; iterative process.

;; Recursive
(define (recursive-proc n)
  (cond ((< n 3) n)
        (else (+ (recursive-proc (- n 1)) (* 2 (recursive-proc (- n 2)))
                 (* 3 (recursive-proc(- n 3)))))))

;; Iterative (INCOMPLETE) Office-Hours?

(define (iterative-proc n)
  (define (func1 n count)
    (cond ((< n 3) (expt n count))
          (else (func1 (- n 1) (+ count 1)))))
  (define (func2 n count)
    (cond ((< n 3) (expt (* 2 n) count))
          (else (func2 (- n 2) (+ count 1)))))
  (define (func3 n count)
    (cond ((< n 3) (expt (* 3 n) count))
          (else (func3 (- n 3) (+ count 1)))))
  (+ (func1 n 1) (func2 n 0) (func3 n 0)))