(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
      (- counter 1)
      (* b product)))) 


(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1
    (even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))


;;;問題ここから

(define (fast-expt b n)
  (expt-iter b n 1))

(define (fast-expt-iter b c a)
  (if (= count 0)
    a
    (cond
      ((even? c) (fast-expt-iter (square b) (/ n 2) a))
      (else (fast-expt-iter b (- n 1) (* a b))))))
