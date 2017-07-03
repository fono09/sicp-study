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
  (cond 
    ((number? exp) 0)
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
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (auegend s) (caddr s))

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
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
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
	  (make-exponentation (base exp) (- (exponent exp) 1)))
	(deriv (base exp) var)))
     ((product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
     (else
      (error "unknown expression type -- DERIV" exp))))

; 2.57
(define (make-sum a1 a2 . a3)
  (let* 
    (
     (a4 (cons a1 (cons a2 a3)))
     (a5 (fold + 0 (filter number? a4)))
     (a6 (filter variable? a4))
     (a7 (filter pair? a4))
     (a8 (filter (lambda (x) (not (null? x))) (append a6 a7))))
    (cond ((null? a8) a5)
	  ((=number? a5 0) (if (> (length a8) 1) (cons '* a8) (car m8)))
	  (else (cons '+ (cons a5 a8))))))

(define (make-product m1 m2 . m3)
  (let*
    (
     (m4 (cons m1 (cons m2 m3)))
     (m5 (fold * 1 (filter number? m4)))
     (m6 (filter variable? m4))
     (m7 (filter pair? m4))
     (m8 (filter (lambda (x) (not (null? x))) (append m6 m7))))
    (cond ((null? m8) m5)
	  ((=number? m5 0) 0)
	  ((=number? m5 1) (if (> (length m8) 1) (cons '* m8) (car m8)))
	  (else (cons '* (cons m5 m8))))))


(define (addend s) (cadr s))
(define (augend s) (let ((s1 (cddr s)))(if (> (length s1) 1) (cons '+ s1) (car s1))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (let ((p1 (cddr p)))(if (> (length p1) 1) (cons '+ p1) (car p1))))


; 2.58-a
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond  ((=number? a1 0) a2)
	 ((=number? a2 0) a1)
	 ((and (number? a1) (number? a2)) (+ a1 a2))
	 (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))


; 2.58-b

(define (select-iter s p v)
  (cond ((null? s) p)
  	((eq? (car s) v) p) 
    	(else (select-iter (cdr s) (append p (list (car s))) v))))

(define (sum? x)
  (memq '+ x))
(define (addend s) 
  (let ((s1 (select-iter s '() '+))) (if (> (length s1) 1) s1 (car s1))))
(define (augend s)
  (let ((s1 (cdr (memq '+ s)))) (if (> (length s1) 1) s1 (car s1))))

(define (product? x)
  (memq '* x))
(define (multiplier p)
  (let ((s1 (select-iter p '() '*))) (if (> (length s1) 1) s1 (car s1))))
(define (multiplicand p)
  (let ((p1 (cdr (memq '* p)))) (if (> (length p1) 1) p1 (car p1))))
