; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.


; Iterative Solution
(define (sum-of-digits integer)
  (define (sum-of-digits-helper integer sum)
    (cond ((= integer 0) sum)
          (else (sum-of-digits-helper (floor (/ integer 10)) (+ sum (remainder integer 10))))))
  (sum-of-digits-helper integer 0))
; Invariant:
;;            (Dn|D(n-1)|...|D2|D1) / 10 = (Dn|D(n-1)|...|D2)
;;            (Dn|D(n-1)|...|D2|D1) % 10 = D1


; Recursive Solution
(define (sum-of-digits-recursive integer)
  (cond ((= integer 0) integer)
        (else (+ (remainder integer 10) (sum-of-digits-recursive (floor (/ integer 10)))))))

;; D3|D2|D1 % 10 = D1
;; D3|D2|D1 / 10 = D3|D2
;; (integer % 10) + (sum-of-digits-recursive (integer / 10))
;;
;; IH:             (sum-of-digits-recursive D3|D2|D1) = D3+D2+D1 where D is a single integer digit, and DN|D(N-1)|...|D1
;;                 is an appending of these digits such that they create an integer.
;;
;; Basis Step:     (sum-of-digits-recursive D1) = D1
;;
;; Inductive Step: We see that after the first call, we have the following: D1 + (sum-of-digits-recursive D3|D2).
;;                 This tail recursive procedure will follow this pattern until we reach the termination case.
;;
;; Termination:    When (sum-of-digits-recursive) is called with integers of multiple digits, we observe that the procedure
;;                 will continue to call itself until we reach the termination case (integer == 0).