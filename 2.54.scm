(define (equal? a b)
  (cond 
    ((eq? a b) #t)
    ((and 
       (pair? a) 
       (pair? b)
       (equal? (car a) (car b)) 
       (equal? (cdr a) (cdr b))) #t)
    (else #f)))
