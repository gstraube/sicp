#lang racket

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin frame)
  (car frame))

(define (first-edge frame)
  (cadr frame))

(define (second-edge-1 frame)
  (caddr frame))

(define (second-edge-2 frame)
  (cddr frame))
