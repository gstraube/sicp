#lang sicp
(#%require sicp-pict)

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(first painter (second smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))


