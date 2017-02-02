(define (same-parity first . rest)
  (define (collect src acc)
    (if (null? src)
      acc
      (if (eq? (even? first) (even? (car src)))
	(collect (cdr src) (append acc (list (car src))))
	(collect (cdr src) acc))))
  (collect rest (list first)))
