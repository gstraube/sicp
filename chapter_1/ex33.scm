(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
      result
      (if (filter a)
	(iter (next a) (combiner result (term a)))
	(iter (next a) result))))
  (iter a null-value))
