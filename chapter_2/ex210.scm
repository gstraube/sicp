(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (extract-bound interval <))

(define (upper-bound interval)
  (extract-bound interval >))

(define (extract-bound interval comparator)
  (if (comparator (car interval) (cdr interval))
    (car interval)
    (cdr interval)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= 0 (- (upper-bound y) (lower-bound y)))
    (display "Error: Trying to divide by an interval with width 0")
    (mul-interval x 
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))
