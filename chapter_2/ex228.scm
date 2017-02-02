(define (fringe tree)
  (define (traverse tree result)
    (if (null? tree)
      result
      (if (pair? (car tree))
	(traverse (cdr tree) (append result (traverse (car tree) '())))
	(traverse (cdr tree) (append result (list (car tree)))))))
  (traverse tree '()))
