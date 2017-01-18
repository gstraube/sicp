(define (close-enough? x)
  (define phi (/ (+ 1 (sqrt 5)) 2))
  (< (abs (- x (/ 1 phi))) 0.00001))

(define (cont-frac n d k)
  (define (calc i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (calc (+ i 1))))))
  (calc 1))

(define (find-needed-precision)
  (define (check k)
    (if (close-enough? (cont-frac
			 (lambda (i) 1.0)
			 (lambda (i) 1.0) k))
      k
      (check (+ k 1))))
  (check 1))
