#lang racket

(require racket/mpair)

(define (make-deque)
  (mcons '() '()))

(define (front-ptr deque)
  (mcar deque))

(define (rear-ptr deque)
  (mcdr deque))

(define (set-front-ptr! deque item)
  (set-mcar! deque item))

(define (set-rear-ptr! deque item)
  (set-mcdr! deque item))

(define (empty? deque)
  (null? (front-ptr deque)))

(define (empty-error)
  (error "Deque is empty"))
  
(define (front-deque deque)
  (if (not (empty? deque))
      (mcar (front-ptr deque))
      (empty-error)))

(define (rear-deque deque)
  (if (not (empty? deque))
      (mcar (rear-ptr deque))
      (empty-error)))

(define (make-new-entry item)
  (mcons item (mcons '() '())))

(define (get-next entry)
  (mcdr (mcdr entry)))

(define (set-next! entry next)
  (set-mcdr! (mcdr entry) next))

(define (get-previous entry)
  (mcar (mcdr entry)))

(define (set-previous! entry previous)
  (set-mcar! (mcdr entry) previous))

(define (get-item entry)
  (mcar entry))

(define (front-insert-deque! deque item)
  (define new-entry (make-new-entry item))
  (define front-entry (front-ptr deque))
  (cond ((empty? deque)
         (set-front-ptr! deque new-entry)
         (set-rear-ptr! deque new-entry))
        (else
         (set-previous! front-entry new-entry)
         (set-next! new-entry front-entry)
         (set-front-ptr! deque new-entry))))

(define (rear-insert-deque! deque item)
  (define new-entry (make-new-entry item))
  (define rear-entry (rear-ptr deque))
  (cond ((empty? deque)
         (set-front-ptr! deque new-entry)
         (set-rear-ptr! deque new-entry))
        (else
         (set-previous! new-entry rear-entry)
         (set-next! rear-entry new-entry)
         (set-rear-ptr! deque new-entry))))

(define (front-delete-deque! deque)
  (cond ((not (empty? deque))
         (set-front-ptr! deque (get-next (front-ptr deque))))
         (set-previous! (front-ptr deque) '())
        (else (empty-error))))

(define (rear-delete-deque! deque)
  (cond ((not (empty? deque))
         (set-rear-ptr! deque (get-previous (rear-ptr deque)))
         (set-next! (rear-ptr deque) '()))
        (else (empty-error))))

(define (print-deque deque)
  (define (print-list list)
    (cond ((not (null? list))
           (display (get-item list))
           (display " ")
           (print-list (get-next list)))))
  (print-list (front-ptr deque)))
