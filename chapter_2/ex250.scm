#lang sicp
(#%require sicp-pict)

(define flip-horiz
  (transform-painter (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
