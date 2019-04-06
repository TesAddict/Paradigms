; Ben Bitdiddle has invented a test to determine whether the interpreter he is
; faced with is using applicative-order evaluation or normal-order evaluation.
; He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then he evaluates the expression (test 0 (p))

; What behavior will Ben observe with an interpreter that uses applicative-order
; evaluation? What behavior will he observe with an interpreter that uses normal
; order evaluation? Explain your answer.

; Answer: If the interpreter is applicative-order, it will get stuck in an infinite
; loop attempting to evaluate (p). This is because applicative-order interpreters
; first substitute all values in, even before they are needed in an evaluation.

; If the interpreter is normal-order interpretation, it will return the value 0,
; because it will not subsistute the value of y unless it is needed. In the provided
; example, the value of y is never needed since the if conditional evaluates to the
; first state of 0. 