(define (remainder a n)
    (- a
        (* (abs n) 
           (floor (/ a 
                     (abs n))))))

(define (gcd a b)
  (if (= b 0)
    	a
	(gcd b (remainder a b))))
