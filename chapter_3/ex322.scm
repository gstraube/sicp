#lang racket

(define (make-queue)
 (let ((front-ptr '())
       (rear-ptr '()))
  (define (empty?)
   (null? front-ptr))
  (define (dispatch m)
   (cond ((eq? m 'empty)
	  (empty?))
    (else (error "Unknown procedure"))))
  dispatch))

