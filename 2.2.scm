(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (midpoint-segment l)
  (make-point 
    (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
    (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(define line (make-segment (make-point 0 0) (make-point 1 1)))
