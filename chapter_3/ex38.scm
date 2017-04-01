#lang racket

(define f
  (let ((value null))
    (lambda (x)
      (if (null? value)
          (begin
            (set! value x)
            value)
          (if (= value 0)
              0
              x)))))
