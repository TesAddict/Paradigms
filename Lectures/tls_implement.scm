(define first
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (car (cdr l))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define (new-entry name value)
  (cons name (cons value '())))


(define (get-names entry)
  (car entry))

(define (get-vals entry)
  (cadr entry))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (get-names entry)
                          (get-vals entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((not (eq? (length names) (length values))) (arity-mismatch))
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


(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f table))
      (else (lookup-in-entry
             name
             (car table)
             (lambda (name)
               (lookup-in-table name (cdr table) table-f)))))))



; What is the type of e where e is 6? -> *const.

; What is (value e) where e is car? -> (primitive car)

; What is the type of e where e is (quote nothing)? -> *quote


; What is the type of d? -> *lambda



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

(define text-of second)

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
         ((eq? (car e) (quote let)) *let)
         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *let
  (lambda (e table)
    (meaning (lambda-from-let e) table)))

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
    (quote ())))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))


(define build
  (lambda (primitive e)
    (cons primitive (cons e '()))))

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

(define arity-mismatch
  (lambda ()
    (display "Arity mismatch has been detected")
    (car '())))

; I want to check to see if a variable has been defined
; before the application attempts to execute. If it has
; not been found, the arity-mismatch procedure should
; execute.

; The initial idea was to first check to see if vals
; and the formals-of closure are the same length.

; Is this a robust enough definition at this level?

; I don't think so, what happens if a formal for a particular
; nested lambda expression was defined at a higher lexical scope?

; I will explore the behavior of R5RS in this event.

; If there is an arity mismatch at any level of the program,
; the problem will become evident after the rib-cage table
; is constructed. Lets use the example of a simple closure:

; ((lambda (a b) (cons a b) 1)

; In the above example, we expect two values for the application of
; this lambda statement(one for a and b). Only a single value is provided.

; When a rib is built based on these parameters, the arity mismatch will
; manifest. Either the value or the name of the variables will be longer.

; We can therefore check for arity mismatches by checking the length of
; the names and variables during the lookup-in-entry-helper procedure.
; If the lengths of these two lists are not equal, we shall call the
; procedure arity-mismatch and terminate execution of the program.

; This works even in the context of inheritence or lexical scoping.
; When the external lambda expression generates it's table, lets
; say in the example:

; '((lambda (f a) (f a)) (lambda (a) (atom? a)) 5))

; The application of the internal lambda function does not begin
; until after the table is established from the external lambda
; function (lambda (f a) (f a)).

; When this internal/nested lambda function is finally executed
; (lambda (a) (atom? a)). It does not go through the *application
; route of the TLS interpreter, but rather only the *lambda route.
; Therefore, it will be turned into -> '((non-primitive lambda) (() (a) (atom? a)).
; When meaning is applied to this expression, it will use the already existing
; rib table and look for a in this table. Because it was already established
; in the outer nested lambda, it will be found no problem.

; To fix the issue with unbound-variables requires a search for a particular
; variable. If it does not exist in the current scope, it is unbounded. 


         
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))


; (primitive cons) (6 (a b c)) paramters of myapply in above example.

; (cons) (6 (a b c)) parameters of apply-primitve.

(define add1
  (lambda (x)
    (+ x 1)))

;(value '((lambda (x) (add1 x)) 3))

;(value '((lambda (x y) (cons x (cons y '()))) 4 5))


(define let-struct '(let ((x 1)(y 5)) (let ((x 5)) (add1 x))))

(define lambda-struct '((lambda (x y) (cons x y)) 1 5))


(define lambda-from-let
  (lambda (lets)
    (cons (list 'lambda (let-vars lets) (let-body lets)) (let-vals lets))))

(define let-params
  (lambda (let-statement)
    (cadr let-statement)))

(define let-vars
  (lambda (let-statement)
    (map car (let-params let-statement))))

(define let-vals
  (lambda (let-statement)
    (map cadr (let-params let-statement))))

(define let-body
  (lambda (let-statement)
    (third let-statement)))

;; (value '((lambda (x) (add1 x)) 3)) 

;; e -> '((lambda (x) (add1 x)) 3))
;; table -> '()

;; 


;; (meaning (expression-to-action e) e table)

;; (*application e table)

;; (myapply (meaning (function-of e) table) (evlis (args-of e))

;; (myapply-closure

;; ((lambda (x y) (cons x y)) 1 2) <- (let ((x 1)(y 2)) (cons x y))

;; (cons (list 'lambda (args of lambda) (body of lambda)) (vals of lambda))








    
    












    
  