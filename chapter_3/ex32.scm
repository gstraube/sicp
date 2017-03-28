#lang racket

(define (make-monitored proc)
  (define counter 0)
  (lambda (arg)
    (cond ((eq? arg 'how-many-calls?) counter)
          ((eq? arg 'reset-count) (set! counter 0))
          (else (begin
                  (set! counter (+ counter 1))
                  (proc arg))))))
