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

(define empty-board null)

(define (safe-diagonally? row other-row)
  (define distance (- (car row) (car other-row)))
  (not (= (abs (- (cdr row) (cdr other-row))) distance)))

(define (row-safe? row positions)
  (if (null? positions)
    #t
    (if (and (not (= (car row) (caar positions)))
	     (or (= (cdr row) (cdar positions))
		 (not (safe-diagonally? row (car positions)))))
      #f
      (row-safe? row (cdr positions)))))

(define (safe? k positions)
  (define (check-rows rows)
    (if (null? rows)
      #t
      (if (not (row-safe? (car rows) positions))
	#f
	(check-rows (cdr rows)))))
  (check-rows
    (filter (lambda (x) (= k (car x)))
	    positions)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons k new-row) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))
