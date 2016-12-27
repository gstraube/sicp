(define (binom n k)
  (cond ((= k 0) 1)
	((= n k) 1)
	(else (+ (binom (- n 1) k) (binom (- n 1) (- k 1)))
