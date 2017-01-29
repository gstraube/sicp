(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (extract-bound interval <))

(define (upper-bound interval)
  (extract-bound interval >))

(define (extract-bound interval comparator)
  (if (comparator (car interval) (cdr interval))
    (car interval)
    (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x 
		(make-interval (- (lower-bound y))
			       (- (upper-bound y)))))
