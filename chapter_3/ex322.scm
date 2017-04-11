#lang racket

(define (make-queue)
 (let ((front-ptr '())
       (rear-ptr '()))
  (define (empty?)
   (null? front-ptr))
  (define (insert-queue! element)
   (define new-pair (mcons element null))
   (cond
    ((empty?)
     (set! front-ptr new-pair)
     (set! rear-ptr new-pair))
    (else
     (set-mcdr! rear-ptr new-pair)
     (set! rear-ptr new-pair))))
  (define (dispatch m)
   (cond ((eq? m 'empty?)
	  (empty?))
    ((eq? m 'insert-queue!)
     insert-queue!)
    (else (error "Unknown procedure"))))
  dispatch))

