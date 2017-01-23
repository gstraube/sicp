(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated proc n)
  (define (iter i result)
    (if (= i n)
      result
      (iter (+ i 1) (compose proc result))))
  (iter 1 (compose proc (lambda (x) x))))
