#lang racket

(define (make-accumulator acc)
  (lambda (value)
    (begin
      (set! acc (+ acc value))
      acc)))
