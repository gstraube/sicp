#lang racket
(require scheme/mpair)
(require racket/set)

(define (count-pairs x)
  (define visited-pairs (mutable-set))
  (define (traverse x)
    (cond ((mpair? x)
	   (set-add! visited-pairs x)
	   (cond
	     ((not (set-member? visited-pairs (mcar x)))
	      (traverse (mcar x)))
	     ((not (set-member? visited-pairs (mcdr x)))
	      (traverse (mcdr x)))))))
  (traverse x)
  (set-count visited-pairs))
