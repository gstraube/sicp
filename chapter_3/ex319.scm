#lang racket
(require scheme/mpair)

(define (has-cycle? x)
  (define (arrives-at-start? p1 p2)
    (cond ((null? p2) #f)
	  ((eq? p2 p1) #t)
	  (else
	    (arrives-at-start? p1 (mcdr p2)))))
  (cond ((null? x) #f)
	((arrives-at-start? x (mcdr x)) #t)
	(else (has-cycle? (mcdr x)))))
