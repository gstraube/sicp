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

(define (new-mul-interval x y)
  (define (pos? x) (> x 0))
  (define (neg? x) (not (pos? x)))
  (cond
    ((and (neg? (lower-bound x)) (neg? (upper-bound x))
	  (neg? (lower-bound y)) (neg? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
		    (* (lower-bound x) (lower-bound y))))))

(define (run-tests)
  (define i1 (new-mul-interval (make-interval (- 20) (- 5))
			       (make-interval (- 3) (- 1))))
  (and
    (and (= (lower-bound i1) 5) (= (upper-bound i1) 60))))
