(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (check-congruity a n)
  (cond ((= a n) #t)
	((not (= (expmod a n n) a)) #f)
	(else (check-congruity (+ a 1) n))))

(define (check-number n) (check-congruity 0 n))
