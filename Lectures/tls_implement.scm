(define mynames '(a b))

(define myvalues '(1 2))

(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define (mylength list)
  (cond ((null? list) 0)
        (else (+ 1 (mylength (cdr list))))))

(define (new-entry name value)
  (cons name (cons value '())))

(define entry '((appetizer entree beverage)(food tastes good)))

(define (mylookup-in-entry name entry)
  (index-to-val (index-of-name name (get-names entry)) (get-vals entry)))

(define (get-names entry)
  (car entry))

(define (get-vals entry)
  (cadr entry))


(define (index-of-name name names)
  (cond ((null? names) 0)
        ((eq? name (car names)) 0)
        (else (+ 1 (index-of-name name (cdr names))))))

(define (index-to-val index vals)
  (cond ((= index 0) (car vals))
        (else (index-to-val (- index 1) (cdr vals)))))

; The Little Schemer Implementation
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (get-names entry)
                          (get-vals entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help
             name
             (cdr names)
             (cdr values)
             entry-f)))))

(define extend-table
  (lambda (entry table)
  (cons entry table)))

(define table-f
  (lambda (name)
    (car (quote ()))))

(define table1 (extend-table entry '()))
(define table2 (extend-table '((a b)(1 2)) table1))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f names))
      (else (lookup-in-entry
             name
             (car table)
             (lambda (name)
               (lookup-in-table name (cdr table) table-f)))))))


(define e
  ((lambda (nothing)
     (cond
       (nothing (quote something))
       (else (quote nothing))))
   #t))

; What is the type of e where e is 6? -> *const.

; What is (value e) where e is car? -> (primitive car)

; What is the type of e where e is (quote nothing)? -> *quote

(define d
  (lambda (x y) (cons x y)))

; What is the type of d? -> *lambda

(define an-app
  ((lambda (nothing)
     (cond
       (nothing (quote something))
       (else (quote nothing))))
   #t))

; What is the type of an-app? -> *application

; How many types are there? ->
; *const
; *quote
; *identifier
; *lambda
; *cond
; *application

; How should we represent types? We choose functions.
; We call these functions "actions".

; If actions are functions that do "the right thing" when
; applied to the appropriate type of expression, what should
; value do?

; It should find out the type of expression it was passed and
; then use the associated action.

(define atom?
  (lambda (list)
    (cond ((and (not (null? list)) (not (pair? list))) #t)
          (else #f))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))


(define build
  (lambda (primitive e)
    (cons primitive (cons e '()))))

(define test-table '(((y z)((8) 9))))
(define test-e '(lambda (x) (cons x y)))

(define test-meaning (meaning test-e test-table))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define myline '((= n 0) 0))

(define question-of
  (lambda (line)
    (car line)))

(define answer-of
  (lambda (line)
    (car (cdr line))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define mycond '(cond (coffee klatsch) (else party)))
(define cond-tab '(((coffee) (#t))
                   ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

; How many different kinds of functions are there? ->
; Two: primitives and non-primitives.



(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define myapply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive)) #t)
      (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

(define test-closure '(((( u v w )
( 1 2 3))
( (x y z)
(4 5 6) ) )
(x y)
( cons z x ) ))

(define test-vals '((a b c) (d e f)))

; (primitive cons) (6 (a b c)) paramters of myapply in above example.

; (cons) (6 (a b c)) parameters of apply-primitve.

(define add1
  (lambda (x)
    (+ x 1)))

;(value '((lambda (x) (add1 x)) 3))

(define expre '((lambda (x) (add1 x)) 3))












    
  