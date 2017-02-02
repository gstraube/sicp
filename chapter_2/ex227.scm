(define (deep-reverse l)
  (define (iter src acc)
    (if (null? src)
      acc
      (if (pair? (car src))
	(iter (cdr src) (cons (deep-reverse (car src)) acc))
	(iter (cdr src) (cons (car src) acc)))))
  (iter l '()))
