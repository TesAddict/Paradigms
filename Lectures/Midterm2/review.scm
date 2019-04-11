(define x (cons 1 (cons 2 '())))

(define y (list 1 2))

(define z (append (list 1) (list 2)))

(define a '(1 2))

(define b (cons 1 2))

(define (atom? element)
  (and (not (pair? element)) (not (null? element))))



(define list2 '(a b c d e f (a (b c) (a e))))

; Flatten list in this context will turn a tree into
;; a flat list. 
(define (flatten-tree tree)
  (cond ((atom? tree) tree)
        ((null? (cdr tree)) (car tree))
        (else (cons (flatten-tree (car tree))
                    (flatten-tree (cdr tree))))))

;; This procedure replaces all instances of old with new
;; in the lst. 
(define (sub-element lst old new)
  (cond ((null? lst) '())
        ((and (atom? lst) (eq? lst old)) new)
        ((atom? lst) lst)
        (else (cons (sub-element (car lst) old new)
                    (sub-element (cdr lst) old new)))))

;; Create a procedure that compares two lists, and checks
;; to see if they are identical.

(define (same-shape? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) #t)
        ((and (atom? lst1) (atom? lst2)) (eq? lst1 lst2))
        (else (and (same-shape? (car lst1) (car lst2))
                   (same-shape? (cdr lst1) (cdr lst2))))))

;; Exercise 2.2: Consider the problem of representing line
;; segments in a plane. Each segment is represented as a
;; pair of points: a starting point and an ending point.
;; Define a constructor make-segment and selectors start-segment
;; and end-segment that define the representation of segments
;; in terms of points. Furthermore, a point can be represented
;; as a pair of numbers: the x coordinate and the y coordinate.
;; Accordingly, specify a constructor make-point and selectors
;; x-point and y-point that define this representation. Finally,
;; using your selectors and constructors, define a procedure
;; midpoint-segment that takes a line segment as argument
;; and returns its midpoint (the point whose coordinates are
;; the average of the coordinates of the endpoints). To try
;; your procedures, you'll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define (make-segment point1 point2))

(define make-+
  (lambda (e1 e2)
    (list e1 '+ e2)))

(define make-*
  (lambda (e1 e2)
    (list e1 '* e2)))

(define make-^
  (lambda (e1 e2)
    (list e1 '^ e2)))

(define test-exp-1
  (make-+
   (make-* 4 5)
   (make-^ 2 3)))

(define operator
  (lambda (i-aexp)
    (car (cdr i-aexp))))

(define first-operand
  (lambda (i-aexp)
    (car i-aexp)))

(define second-operand
  (lambda (i-aexp)
    (car (cdr (cdr i-aexp)))))

;; classifiers for i-aexps

(define plus-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '+)))

(define times-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '*)))

(define power-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '^)))

;; This is a common pattern for new data types: define constructors,
;; selectors and then classifiers.

(define value
  (lambda (aexp)
    (cond ((natnum? aexp) aexp)
          (else (cond ((plus-aexp? aexp) (plus (value (first-operand aexp))
                                               (value (second-operand aexp))))
                      ((times-aexp? aexp) (times (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      ((power-aexp? aexp) (expon (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      )))))

; natnum? might be:

(define natnum?
  (lambda (x)
    (and
     (integer? x)
     (>= x 0))
    ))

; here integer? and >= are pre-defined in scheme


; one possible implementation of plus, times and expon

(define plus 
  (lambda (m n)
    (cond ((= n 0) m)
          (else (add1 (plus m (sub1 n)))))))

(define times 
  (lambda (m n)
    (cond ((= n 0) 0)
          (else (plus m (times m (sub1 n)))))))

(define expon
  (lambda (base exponent)
    (cond ((zero? exponent) 1)
          (else (times base (expon base (sub1 exponent)))))))

; where

(define add1
  (lambda (m) (+ m 1)))

(define sub1
  (lambda (m) (- m 1)))

;; Return the element at the index. 
(define (my-list-ref list index)
  (cond ((= index 0) (car list))
        ((null? list) '(index out of bounds))
        (else (my-list-ref (cdr list) (- index 1)))))

;; Return everything except the last element in the list. 
(define (all-but-last list)
  (cond ((null? (cdr list)) '())
        (else (cons (car list) (all-but-last (cdr list))))))


;; Add an element to the end of a list.
(define (my-append list element)
  (cond ((and (null? list) (atom? element)) (cons element '()))
        ((null? list) element)
        (else (cons (car list) (my-append (cdr list) element)))))

;; Take a list and reverse it.
(define (my-reverse lst)
  (cond ((null? lst) lst)
        (else (my-append (my-reverse (cdr lst)) (list (car lst))))))

(define (square x) (* x x))

(define num-list '(1 2 3 4 (5 6)))

;; my-map only applies to flat lists. 
(define (my-map function list)
  (cond ((null? list) '())
        (else (cons (function (car list)) (my-map function (cdr list))))))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (filter pred seq)
  (cond ((null? seq) '())
        ((and (atom? seq) (pred seq)) seq)
        ((atom? seq) (list '()))
        (else (cons
               (filter pred (car seq))
               (filter pred (cdr seq))))))

(define (my-odd? x)
  (cond ((equal? (modulo x 2) 0) #f)
        (else #t)))

(define (fringe lst)
  (cond ((atom? lst) (list lst))
        ((null? lst) '())
        (else (my-append
               (fringe (car lst))
               (fringe (cdr lst))))))

;;  (a (b c) (d e))
(define list1 '(a b c d e f (a (b c) (a d))))

(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((not (atom? (car lst)))
         (my-append
          (deep-reverse (cdr lst))
          (list (deep-reverse (car lst)))))
        (else
         (my-append
          (deep-reverse (cdr lst))
          (list (car lst))))))



