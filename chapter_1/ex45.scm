(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log_2 x) 
  (/ (log x) (log 2)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated proc n)
  (define (iter i result)
    (if (= i n)
      result
      (iter (+ i 1) (compose proc result))))
  (iter 1 (compose proc (lambda (x) x))))

(define (nth-root x n)
  (let ((proc (lambda (y) (/ x (expt y (- n 1)))))
	(num-rep (truncate (log_2 n))))
    (fixed-point ((repeated average-damp num-rep) proc) 1.0)))
