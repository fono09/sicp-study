(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))


(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define (make-rat n d)
  (gcd-rat (normalize-rat (cons n d))))

(define (gcd-rat x)
  (let ((g (gcd (numer x) (denom x))))
    (cons (/ (numer x) g) (/ (denom x) g))))

(define (normalize-rat x)
    (if 
      (>= (* (numer x) (denom x)) 0) 
      (cons (abs (numer x)) (abs (denom x)))
      (cons (- (abs (numer x))) (abs (denom x)))))
