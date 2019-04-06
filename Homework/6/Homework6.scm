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
; Abelson and Sussman, Exercise 2.29
; Abelson and Sussman, Exercise 2.32
; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







