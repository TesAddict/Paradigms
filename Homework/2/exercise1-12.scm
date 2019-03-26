;; The following pattern of numbers is called Pascal's triangle.
;;                   1
;;                1  2  1
;;              1  3   3  1
;;            1  4   6   4  1
;;               .........

;; The numbers at the edge of the triangle are all 1, and each
;; number inside the traingle is the sum of the two numbers
;; above it. Write a procedure that computes elements of Pascal's
;; triangle by menas of a recursive process.

(define (pascal row col)
  (cond ((> col row) 0)
        ((>= 0 col) 0)
        ((>= 0 row) 0)
        ((= col 1) 1)
        ((= col row) 1)
        (else ( + (pascal (- row 1) (- col 1))
                  (pascal (- row 1) col)))))


;;   | 1 | 2 | 3 | 4 | 5  
;; ----------------------
;; 1 | 1 |   |   |   |
;; ----------------------
;; 2 | 1 | 1 |   |   |
;; ----------------------
;; 3 | 1 | 2 | 1 |   |
;; ----------------------
;; 4 | 1 | 3 | 3 | 1 |
;; ----------------------
;; 5 | 1 | 4 | 6 | 4 | 1

(define (pascal-iter row col)
  
  (define (pascal-iter-helper row col value i)
    (cond ((>= i col) value)
          (else (pascal-iter-helper row col (/ (* value (- row i)) (+ i 1)) (+ i 1)))))

  (pascal-iter-helper row col 1 0))



