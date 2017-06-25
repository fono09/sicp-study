#|
(variable? e) ;変数か
(same-variable? v1 v2) ;同じ変数か

(sum? e) ;eは和か.
(addend e) ;eの加数.
(augend e) ;eの被加数.
(make-sum a1 a2) ;a1とa2の和を構成.

(product? e) ;eは積か.
(multiplier e) ;eの乗数.
(multiplicand e) ;eの被乗数.
(make-product m1 m2) ;m1とm2の積を構成.
|#

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp)
      (if (same-variable? exp var) 1 0))
     ((sum? exp)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
     ((product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
     (else
      (error "unknown expression type -- DERIV" exp))))

(define variable? symbol?)
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) 'x)))
(define (addend s) (cadr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
  
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; 2.56

(define (exponentation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentation b e)
  (cond ((= number? e 0) 1)
	((= number? e 1) b)
	(else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp)
      (if (same-variable? exp var) 1 0))
     ((sum? exp)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
     ((exponentation? exp)
      (make-product
        (make-product 
	  (exponent exp)
	  (make-exponentation (base exp) (- (exponent exp) 1))
	(deriv (base exp) var))))
     ((product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
     (else
      (error "unknown expression type -- DERIV" exp))))

; 2.57

