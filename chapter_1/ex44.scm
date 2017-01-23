(define dx 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated proc n)
  (define (iter i result)
    (if (= i n)
      result
      (iter (+ i 1) (compose proc result))))
  (iter 1 (compose proc (lambda (x) x))))

(define (smooth proc)
  (lambda (x) (/ (+ (proc (- x dx))
		    (proc x)
		    (proc (+ x dx)))
		 3)))

(define (smooth-n-times proc n)
  (repeated (smooth proc) n))
