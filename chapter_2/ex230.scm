#lang racket

(define (square-tree-1 tree)
  (cond ((null? tree)
         null)
        ((not (pair? tree))
         (expt tree 2))
        (else
         (cons
          (square-tree-1 (car tree))
          (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (expt sub-tree 2)))
       tree))