(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! ptr) (set! front-ptr ptr))
    (define (set-rear-ptr! ptr) (set! rear-ptr ptr))
    (define (empty-deque?) (null? front-ptr))
    (define (lastone-deque?) (eq? front-ptr rear-ptr))
    (define (front-deque)
      (if (empty-deque?)
        (error "FRONT called with an empty deque")
        (cadr front-ptr)))
    (define (rear-deque)
      (if (empty-deque?)
        (error "FRONT called with an empty deque")
        (cdr rear-ptr)))
    (define (print-deque)
      (if (empty-deque?)
        #f
        (print-deque-iter front-ptr)))
    (define (print-deque-iter cptr)
      (print (cadr cptr))
      (if (eq? cptr rear-ptr) #t
        (print-deque-iter (caddr cptr))))
    (define (insert-front-deque! obj)
      (let ((new-item (list '() obj front-ptr)))
        (cond ((empty-deque?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item))
              (else
                (set-car! front-ptr new-item)
                (set-front-ptr! new-item)))))
    (define (insert-rear-deque! obj)
      (let ((new-item (list rear-ptr obj '())))
        (cond ((empty-deque?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item))
              (else
                (set-car! (cddr rear-ptr) new-item)
                (set-rear-ptr! new-item)))))
    (define (delete-front-deque!)
      (cond ((empty-deque?)
             (error "DELETE! called with an empty deque" deque))
            (else
              (if (lastone-deque?) 
                (set-rear-ptr! '())
                (set-car! (caddr front-ptr) '()))
              (set-front-ptr! (caddr front-ptr)))))
    (define (delete-rear-deque!)
      (cond ((empty-deque?)
             (error "DELETE! called with an empty deque" deque))
            (else
              (if (lastone-deque?)
                (set-front-ptr! '())
                (set-car! (cddar rear-ptr) '()))
              (set-rear-ptr! (car rear-ptr)))))
    (define (dispatch m)
      (cond 
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'empty-deque?) empty-deque?)
        ((eq? m 'print-deque) print-deque)
        ((eq? m 'front-deque) front-deque)
        ((eq? m 'set-front-ptr!) set-front-ptr)
        ((eq? m 'set-rear-ptr!) set-rear-ptr)
        ((eq? m 'insert-front-deque!) insert-front-deque!)
        ((eq? m 'insert-rear-deque!) insert-rear-deque!)
        ((eq? m 'delete-front-deque!) delete-front-deque!)
        ((eq? m 'delete-rear-deque!) delete-rear-deque!)
        (else (error "Undefined Opration -- deque:" m))))
    dispatch))

(define (front-ptr z) (z 'front-ptr))
(define (rear-ptr z) (z 'rear-ptr))
(define (empty-deque? z) ((z 'empty-deque?)))
(define (print-deque z) ((z 'print-deque)))
(define (set-front-ptr! z ptr) 
  ((z 'set-front-ptr!) ptr))
(define (set-rear-ptr z ptr) 
  ((z 'set-rear-ptr!) ptr))
(define (set-front-deque! z obj)
  ((z 'set-front-deque!) obj))
(define (set-rear-deque! z obj) 
  ((z 'set-rear-deque!) obj))
(define (insert-front-deque! z obj) 
  ((z 'insert-front-deque!) obj))
(define (insert-rear-deque! z obj) 
  ((z 'insert-rear-deque!) obj))
(define (delete-front-deque! z) ((z 'delete-front-deque!)))
(define (delete-rear-deque! z) ((z 'delete-rear-deque!)))

(define q1 (make-deque))
(insert-front-deque! q1 'b)
(insert-rear-deque! q1 'c)
(insert-rear-deque! q1 'd)
(insert-front-deque! q1 'a)
(delete-rear-deque! q1)
(delete-front-deque! q1)
(delete-front-deque! q1)
(delete-rear-deque! q1)
(print-deque q1)
