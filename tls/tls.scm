(define (atom? element)
  (and (not (null? element)) (not (pair? element))))

; A lat is a list wherein every s-expression within it is an atom.
; That is, no nested lists are allowed. 
(define (lat? list)
  (cond ((null? list) #t)
        ((atom? list) #f)
        ((not (atom? (car list))) #f)
        (else (and (lat? (cdr list))))))

; TLS Implementation
(define lat-tls?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat-tls? (cdr l)))
      (else #f))))