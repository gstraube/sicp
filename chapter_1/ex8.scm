(define (improve guess x)
  (/ (+ (/ x (expt guess 2)) (* 2 guess)) 3))

(define (good-enough? new-guess old-guess)
  (< (/ (abs (- new-guess old-guess)) new-guess) 0.001))

(define (cbrt-iter guess x)
  (if (good-enough? (improve guess x) guess)
    guess
    (cbrt-iter (improve guess x)
	       x)))

(define (cbrt x) (cbrt-iter 1.0 x))
