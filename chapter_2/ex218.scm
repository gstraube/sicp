(define (reverse l)
  (define (iter src acc)
    (if (null? src)
      acc
      (iter (cdr src)
	    (cons (car src) acc))))
  (iter l '()))
