#lang racket
(require scheme/mpair)
(require racket/set)

(define (count-pairs x)
  (define visited-pairs (mutable-set))
  (define (traverse x)
    (cond ((or (not (mpair? x)) (set-member? visited-pairs x)) 0)
	  (else
	    (set-add! visited-pairs x)
	    (+ (traverse (mcar x))
	       (traverse (mcdr x))
	       1))))
  (traverse x))

