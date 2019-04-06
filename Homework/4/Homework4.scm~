
; Fourth Homework Set
; CSc 335
; spring 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.

(define (my-length-iter counter list)
  (cond ((null? list) counter)
        (else (my-length-iter (+ counter 1) (cdr list)))))

;; Precondition: Any list
;; Guess Invariant 1: length-of-list-L = length-of-list-l + count
;; where L is initial list and l is the list during current call.
;; if list = '(a b c d) then 4 = 3 + 1 after first call to my-length-iter
;; if list = '(a b c d) then 4 = 0 + 4 on last call of my-length-iter. 

(define (my-length list)
  (cond ((null? list) 0)
        (else (+ 1 (my-length (cdr list))))))


; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?

;; In this example, I need to write my own version of list ref. If a list such as '(a b c d) is passed
;; I must return the nth paramter of this list. Example (my-list-ref '(a b c d) 2) = c because of
;; zero indexing.

;; Guess Implementation:
;; Take the cdr of the list n times, and on the nth call return the car of the list.

(define (my-list-ref list n)
  (cond ((= n 0) (car list))
        (else (my-list-ref (cdr list) (- n 1)))))



; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

;; I will cons the car of current list to previous list. On recursive call cdr the current
;; list. Have counter for number of elements we need, when counter = 0 return the list
;; constructed with cons.

(define (start lst num)
  (cond ((= num 0) '())
        (else (cons (car lst) (start (cdr lst) (- num 1))))))



; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

