(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; (count-change 10)
;; (cc 10 5)
;; (cc 10 5) => (cc 10 4)(cc -40 5) Count: 0
;; (cc 10 4) => (cc 10 3) (cc -15 4) || (cc -40 5) => 0 Count: 0
;; (cc 10 3) => (cc 10 2) (cc 0 3) || (cc -15 4) => 0 Count: 0
;; (cc 10 2) => (cc 10 1) (cc 5 2) || (cc 0 3) => 1 Count: 1
;; (cc 10 1) => (cc 10 0) (cc 9 1) || (cc 5 2) => (cc 5 1) (cc 0 2) Count: 1
;; (cc 10 0) => 0 || (cc 9 1) => (cc 9 0) (cc 8 1) || (cc 5 1) => (cc 5 0) (cc 4 1) || (cc 0 2) => 1 Count: 2
;; (cc 9 0) => 0 || (cc 8 1) => (cc 8 0) (cc 7 1) || (cc 5 0) => 0 || (cc 4 1) => (cc 4 0) (cc 3 1) Count: 2
;; (cc 8 0) => 0 || (cc 7 1) => 1 EVENTUALLY || (cc 3 1) => 1 EVENTUALLY Count: 4
;; Therefore, 1 + 1 + 1 + 1 = 4
;; 4 ways to count change to 10 cents.
;; Note (cc x 1) Will eventually converge to 1. It becomes (cc x 0) and (cc (x-1) 1). (cc x 0) is equal to 0.
;; Therefore (cc(x-1) 1) => (cc (x-1) 0) (cc (x-2) 1) Where eventually we will have x-x=0 which would yield
;; (cc 0 1) = 1.