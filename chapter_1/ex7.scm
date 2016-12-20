(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? new-guess old-guess)
  (< (/ (abs (- new-guess old-guess)) new-guess) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? (improve guess x) guess)
    guess
    (sqrt-iter (improve guess x)
	       x)))

(define (sqrt x) (sqrt-iter 1.0 x))
