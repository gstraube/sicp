#lang racket

(define proc-table (make-hash '()))

(define (division tagged-element) (car tagged-element))
(define (element tagged-element) (cadr tagged-element))

(define (get-record key tagged-file)
  ((hash-ref proc-table
             (list 'get-record (division tagged-file)))
   key
   (element tagged-file)))

(define (get-salary tagged-record)
  ((hash-ref proc-table
             (list 'get-salary (division tagged-record)))
   (element tagged-record)))

(define (find-employee-record key files)
  (cond ((null? files)'())
        ((not (null? (get-record key (car files))))
         (get-record key (car files)))
        (else (find-employee-record key (cdr files)))))
