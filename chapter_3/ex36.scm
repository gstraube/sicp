#lang racket

(define (gen-rand x)
  (modulo (+ (* 1664525 x) 1013904223) (expt 2 32)))

(define rand
  (let ((seed 4342342))
    (lambda (method)
      (cond ((eq? method 'generate)
	     (begin
	       (define value (gen-rand seed))
	       (set! seed value)
	       value))
	    ((eq? method 'reset)
	     (lambda (new-seed)
	       (set! seed new-seed)))
	    (else (error "Unknown method"))))))
