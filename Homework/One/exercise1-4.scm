; Observe that our model of evaluation allows for combinations whose operators are
; compound expressions. Use this observation to describe the behavior of the following
; procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; If the value of b is negative, the procedure that is applied to a and b is the subtraction
; procedure. This has he same result as taking the absolute value of b + a, because a - (-b) is
; equivalent to a + b. This shows that Scheme has the ability to pick procedures and not only
; literals in conditional statements. 