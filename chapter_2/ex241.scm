#lang racket

(define (enumerate-interval low high)
  (if (> low high)
    null
    (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (make-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
		 (map (lambda (k)
			(list i j k))
		      (enumerate-interval 1 (- j 1))))
	       (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (is-equal-to? s)
  (lambda (triple)
    (= s (+ (car triple) (cadr triple) (caddr triple)))))

(define (find-triples n s)
  (filter (is-equal-to? s) (make-triples n)))

