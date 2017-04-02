#lang racket
(require scheme/mpair)

(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

; count-pairs returns 3
(define a1 (mcons 'a null))
(define a2 (mcons a1 null))
(define a (mcons a2 null))

; count-pairs returns 4
(define b1 (mcons 'b null))
(define b2 (mcons b1 b1))
(define b (mcons b2 null))

; count-pairs returns 7
(define c1 (mcons 'c null))
(define c2 (mcons c1 c1))
(define c (mcons c2 c2))

; count-pairs never returns
(define d1 (mcons 'd null))
(define d2 (mcons d1 null))
(define d (mcons d2 null))
(set-mcar! d1 d)
