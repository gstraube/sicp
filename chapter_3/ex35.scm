#lang racket

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 (+ x2 1)))
          (y (random-in-range y1 (+ y2 1))))
          (predicate x y)))
  (define area-rect (* (- x2 x1) (- y2 y1)))
  (define fraction (exact->inexact (monte-carlo trials experiment)))
  (* fraction area-rect))

(define (estimate-pi trials)
  (define radius 1000)
  (define squared-radius (* radius radius))
  (define (predicate x y)
    (<= (+ (expt x 2) (expt y 2)) squared-radius))
  (/ (estimate-integral predicate (- radius) (- radius) radius radius trials)
     squared-radius))
