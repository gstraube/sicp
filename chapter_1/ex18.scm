(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-mult a b)
  (define (iter prod sum a b)
    (cond ((= a 1) (+ prod sum))
	  ((even? a) (iter (double prod) sum (halve a) b))
	  (else (iter prod (+ sum prod) (- a 1) b))))
  (iter b 0 a b))
