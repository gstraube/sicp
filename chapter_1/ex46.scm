(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve-guess guess))))
  iter)

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess) (< (abs (- (expt guess 2) x)) 0.001))
  (define (improve-guess guess) (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess) (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve good-enough? f) first-guess))
