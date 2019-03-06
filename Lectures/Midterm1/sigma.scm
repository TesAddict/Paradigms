(define (sigma a b sum)
  (cond ((= a (+ b 1)) sum)
        (else (sigma (+ a 1) b (+ a sum)))))

;; Invariant: result-so-far = sum-of-A-to-i  where A <= i < a
;; Termination: ((a > b) and (a <= b+1)) or a = b+1
;; Precondition: 0 <= a <= b
;;
;; Does the invariant hold after the first call?
;; 


(define (sigma-recur a b)
  (cond ((= a (+ b 1)) 0)
        (else (+ a (sigma-recur (+ a 1) b)))))

(define (sigma-recur-term a b term)
  (cond ((= a (+ b 1)) 0)
         (else (+ (term a) (sigma-recur-term (+ a 1) b term)))))

(define (square x) (* x x))

(define (accumulate combiner init a next b term)
  (cond ((> a b) init)
        (else
         (combiner (term a)
                   (accumulate combiner init (+ a 1) next b term)))))

(define (sigma-mod a b)
  (accumulate (lambda (x y) (+ x y)) 0 a (lambda (x) (+ x 1)) b (lambda (x) x)))



