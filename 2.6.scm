(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
 
(define one (lambda(f) (lambda(x) (f x))))
(define two (lambda(f) (lambda(x) (f (f x)))))

(define to_i (lambda(n) ((n (lambda(x)(+ x 1)))0)))
(define (add a b) (lambda (f) (lambda(x) ((a f) ((b f) x)))))

