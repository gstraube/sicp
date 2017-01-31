(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (extract-bound interval <))

(define (upper-bound interval)
  (extract-bound interval >))

(define (extract-bound interval comparator)
  (if (comparator (car interval) (cdr interval))
    (car interval)
    (cdr interval)))

(define (pos? x) (> x 0))
(define (neg? x) (not (pos? x)))

(define (handle-cases-1 x y)
  (if (and (neg? (lower-bound y)) (pos? (upper-bound y)))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (lower-bound x) (lower-bound y)))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (upper-bound x) (lower-bound y)))))

(define (handle-cases-2 x y)
  (if (and (neg? (lower-bound y)) (pos? (upper-bound y)))
    (make-interval (min (* (lower-bound x) (upper-bound y))
			(* (lower-bound y) (upper-bound x)))
		   (* (lower-bound x) (lower-bound y)))
    (make-interval (* (lower-bound x) (upper-bound y))
		   (* (upper-bound x) (upper-bound y)))))

(define (mul-interval x y)
  (cond
    ((and (neg? (lower-bound x)) (neg? (upper-bound x))
	  (neg? (lower-bound y)) (neg? (upper-bound y)))
     (make-interval (* (upper-bound x) (upper-bound y))
		    (* (lower-bound x) (lower-bound y))))
    ((and (neg? (lower-bound x)) (neg? (upper-bound x)))
     (handle-cases-1 x y))
    ((and (neg? (lower-bound y)) (neg? (upper-bound y)))
     (handle-cases-1 y x))
    ((and (neg? (lower-bound x)) (pos? (upper-bound x)))
     (handle-cases-2 x y))
    ((and (neg? (lower-bound y)) (pos? (upper-bound y)))
     (handle-cases-2 y x))
    (else
      (make-interval (* (upper-bound x) (upper-bound y))
		     (* (lower-bound x) (lower-bound y))))))

(define (run-tests)
  (define i1 (mul-interval (make-interval (- 20) (- 5))
			   (make-interval (- 3) (- 1))))
  (define i2 (mul-interval (make-interval (- 20) (- 5))
			   (make-interval (- 3) 1)))
  (define i3 (mul-interval (make-interval (- 20) (- 5))
			   (make-interval 3 1)))
  (define i4 (mul-interval (make-interval (- 20) 5)
			   (make-interval (- 3) 1)))
  (define i5 (mul-interval (make-interval 20 5)
			   (make-interval 3 1)))
  (and
    (and (= (lower-bound i1) 5) (= (upper-bound i1) 60))
    (and (= (lower-bound i2) -20) (= (upper-bound i2) 60))
    (and (= (lower-bound i3) -60) (= (upper-bound i3) -5))
    (and (= (lower-bound i4) -20) (= (upper-bound i4) 60))
    (and (= (lower-bound i5) 5) (= (upper-bound i5) 60))))
