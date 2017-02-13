#lang sicp
(#%require sicp-pict)

(define flip-horiz
  (transform-painter (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define rotate90
  (transform-painter (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated proc n)
  (define (iter i result)
    (if (= i n)
      result
      (iter (+ i 1) (compose proc result))))
  (iter 1 (compose proc (lambda (x) x))))

(define rotate180
  (repeated rotate90 2))

(define rotate270
  (repeated rotate90 3))
