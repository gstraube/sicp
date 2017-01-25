(define (make-rat n d)
  (define g (gcd n d))
  (define numer (/ n g))
  (define denom (/ d g))
  (cond ((< denom 0)
	 (cons (- numer) (- denom)))
	(else (cons numer denom))))
