(define (make-rat n d)
  (define g (gcd n d))
  (define numer (/ n g))
  (define denom (/ d g))
  (if (< denom 0)
    (cons (- numer) (- denom))
    (cons numer denom)))
