#lang racket
(require scheme/mpair)

(define (has-cycle? x)
  (define (traverse x visited-pairs)
    (cond ((null? x) #f)
	  ((set-member? visited-pairs x) #t)
	  (else (traverse (mcdr x) (set-add visited-pairs x)))))
  (traverse x (set)))
