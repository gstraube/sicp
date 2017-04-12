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
  (define (front-queue)
   (cond ((not (empty?))
	  (mcar front-ptr))
    (else (error "Queue is empty"))))
  (define (delete-queue)
   (cond ((not (empty?))
	  (set! front-ptr (mcdr front-ptr)))
    (else (error "Queue is empty"))))
(define (dispatch m)
 (cond ((eq? m 'empty?)
	(empty?))
  ((eq? m 'insert-queue!)
   insert-queue!)
  ((eq? m 'front-queue)
   (front-queue))
  ((eq? m 'delete-queue)
   (delete-queue))
  (else (error "Unknown procedure"))))
	dispatch))

(define (empty? queue)
 (queue 'empty?))

(define (insert-queue! queue element)
 ((queue 'insert-queue!) element))

(define (front-queue queue)
 (queue 'front-queue))

(define (delete-queue! queue)
 (queue 'delete-queue))

