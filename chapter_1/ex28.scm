(define (non-triv-root? x n)
  (not (or (= x 1) (= x (- n 1))))) 

(define (square x n)
  (define squared-num (* x x))
  (define mod-num (remainder squared-num n))
  (cond ((= x 0) 0)
	((and (= mod-num 1) (non-triv-root? (remainder x n) n)) 0)
	(else squared-num)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m) m)
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (not (= (expmod a (- n 1) n) 0)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else false)))
