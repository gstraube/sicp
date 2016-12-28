(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= a 1) b)
	((even? a) (double (fast-mult (halve a) b)))
	(else (+ b (double (fast-mult (halve (- a 1)) b))))))
