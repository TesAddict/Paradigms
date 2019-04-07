; Sixth Homework Set
; CSc 335
; Spring 2019

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)

(define my-list '((a b (a d)) (a f)))

(define (atom? element)
  (cond ((and (not (null? element)) (not (pair? element))) #t)
        (else #f)))

(define (traversal lst spc-lst)
  (cond ((null? lst) '())
        ((null? spc-lst) '())
        ((atom? lst) (car spc-lst))
        (else (cons (traversal (car lst) spc-lst) (traversal (cdr lst) spc-lst)))))

(define (fringe lst)
  (cond ((atom? lst) (list lst))
        ((null? lst) '())
        (else (append (fringe (car lst))
              (fringe (cdr lst))))))

(define (mod-nth lst n old new)
  (cond ((null? lst) '())
        ((and (= n 1) (eq? (car lst) old)) (cons new (mod-nth (cdr lst) (- n 1) old new)))
        ((eq? (car lst) old) (cons (car lst) (mod-nth (cdr lst) (- n 1) old new)))
        (else (cons (car lst) (mod-nth (cdr lst) n old new)))))


  


  


; Additional Problems

; Abelson and Sussman, Exercise 2.27

;; Modify your reverse procedure of Exercise 2.18 to produce
;; a deep-reverse procedure that takes a list as argument and
;; returns as its value the list with its elements reversed and
;; with all sublists deep-reversed as well.

;; For example: (define x (list (list 1 2) (list 3 4)))
;; x
;; ((1 2) (3 4))
;; (reverse x)
;; ((3 4) (1 2))
;; (deep-reverse x)
;; ((4 3) (2 1))

(define x '(a b c d e f g))

(define y '((1 2) (3 4)))

;; precondition: (my-reverse '(a b c d e f g))
;; postcondition: '(g f e d c b a)

;; This is accomplished by cdr-ing down the list on the
;; and appending the car of the lst to the right-hand
;; side of the cdr of list. 

(define (my-reverse lst)
  (cond ((null? lst) '())
        (else (my-append (my-reverse (cdr lst)) (list (car lst))))))

(define (my-append lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? (cdr lst1)) (cons (car lst1) lst2))
        (else (cons (car lst1) (my-append (cdr lst1) lst2)))))

(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (my-append
          (deep-reverse (cdr lst))
          (list (deep-reverse (car lst)))))
        (else
         (my-append
          (deep-reverse (cdr lst))
          (list (car lst))))))

; Abelson and Sussman, Exercise 2.29

;; A binary mobile consists of two branches, a left branch
;; and aright branch. Each branch is a rod of a certain length,
;; from which hangs either a weight or an other binary mobile.
;; We can represent a binary mobile using compound data by
;; constructing it from two branches. (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number (representing
;; a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors left-branch and
;;    right branch, which return the branches of a mobile,
;;    and branch-length and branch structure, which return
;;    the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define x1 '((2 3) (4 5)))

;; b. Using your selectors, define a procedure total-weight
;;    that returns the total weight of a mobile.

(define m1 (make-mobile 
             (make-branch 4 6) 
             (make-branch 5 
                          (make-mobile 
                           (make-branch 3 7) 
                           (make-branch 9 8)))))

(define (total-weight mobile)
  (cond ((atom? mobile) mobile)
        ((null? mobile) 0)
        (else (+
               (total-weight (branch-structure (left-branch mobile)))
               (total-weight (branch-structure (right-branch mobile)))
               ))))

;; c. A mobile is said to be balanced if the torque applied by
;;    its top-left branch is equal to that applied by its top-
;;    right branch (that is, if the length of the left rod
;;    multiplied by the weight hanging from that rod is equal
;;    to the corresponding product for the right side) and if
;;    each of the submobiles hanging off its branches is balanced.
;;    Design a predicate that tests whether a binary mobile is
;;    balanced.

;;; If (atom? (left-branch m1)) then
;;; (* (branch-structure m1) (branch-length m1))
;;; ((5 10) (5 ((3 7) (7 3))))




; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







