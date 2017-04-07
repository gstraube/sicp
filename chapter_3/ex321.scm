#lang racket
(require scheme/mpair)

(define (print-queue queue)
  (define (print-items l)
    (cond ((not (null? l))
	   (display (mcar l))
	   (display " ")
	   (print-items (mcdr l)))))
  (print-items (front-ptr queue)))

