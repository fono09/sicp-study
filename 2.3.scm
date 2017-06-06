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

(define make-rectangle cons)

(define point-a car)
(define (point-b rec)
  (make-point (x-point (point-c rec)) (y-point (point-a rec))))
(define point-c cdr)
(define (point-d rec)
  (make-point (x-point (point-a rec)) (y-point (point-c rec))))

(define (distance p q)
  (sqrt (+ (square (abs (- (x-point p) (x-point q))))
	   (square (abs (- (y-point p) (y-point q)))))))

(define (perimeter rec)
  (* (+ (distance (point-a rec) (point-b rec))
	(distance (point-b rec) (point-c rec)))
     2))

(define (area rec)
  (* (distance (point-a rec) (point-b rec))
     (distance (point-b rec) (point-c rec))))
