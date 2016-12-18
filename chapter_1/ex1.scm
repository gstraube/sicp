(define (square x) (* x x))

(define (squared-sum x y z)
  (- (+ (square x)
	(square y)
	(square z))
     (square (min x y z)))

