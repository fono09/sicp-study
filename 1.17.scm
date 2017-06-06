(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (* a n)
  (cond ((= n 0) a)
    ((even? n) (double (* a (halve n)))
    (else (+ a (* a (- n 1)))))))


