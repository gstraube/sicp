#lang racket

(define (square-list items)
  (if (null? items)
    null
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))
