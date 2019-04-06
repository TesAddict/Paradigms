;; The following procedure computes a mathematical function
;; called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1 ))))))

;; What are the values of the following expressions?

(A 1 10)
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

(A 2 4)
;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; . Pattern of (A 1 10) repeats itself. 
;; 1    2    3    4     5     6     7      8      9       10      11      12      13       14       15
;; 1 -> 2 -> 4 -> 16 -> 32 -> 64 -> 128 -> 512 -> 1024 -> 2048 -> 4096 -> 8192 -> 16384 -> 32768 -> 65536
(A 3 3)

(define (f n) (A 0 n)) ;; 2*n
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; 2^2^...(n-1 times)
(define (k n) (* 5 n n))