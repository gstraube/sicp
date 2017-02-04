#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (branch-weight branch)
    (if (list? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (check-balance mobile)
  (define (combine b1 b2 w1 w2)
    (let ([t1 (* w1 (car b1))]
          [t2 (* w2 (car b2))])
    (cons (+ t2 t2) (and (= t1 t2) (cdr b1) (cdr b2)))))
  (define (branch-torque branch)
    (if (list? (branch-structure branch))
        (check-balance (branch-structure branch))
        (cons (branch-structure branch) #t)))
  (combine (branch-torque (left-branch mobile))
           (branch-torque (right-branch mobile))
           (branch-length (left-branch mobile))
           (branch-length (right-branch mobile))))

(define (balanced? mobile)
  (cdr (check-balance mobile)))