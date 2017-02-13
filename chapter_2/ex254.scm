#lang racket

(define (equal? list1 list2)
  (if (and (pair? list1) (pair? list2))
      (and
       (equal? (car list1) (car list2))
       (equal? (cdr list1) (cdr list2)))
      (eq? list1 list2)))
