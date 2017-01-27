(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-rectangle p1 p2 p3)
  (cons p1 (cons p2 p3)))

(define (perimeter rectangle)
  (+ (* 2 (height rectangle)) (* 2 (width rectangle))))

(define (area rectangle)
  (* (height rectangle) (width rectangle)))

(define (width rectangle)
  (distance (car rectangle) (car (cdr rectangle))))

(define (height rectangle)
  (distance (car rectangle) (cdr (cdr rectangle))))

(define (distance p1 p2)
  (sqrt (+ (expt (diff p1 p2 x-point) 2)
	   (expt (diff p1 p2 y-point) 2))))

(define (diff p1 p2 selector)
  (- (selector p1) (selector p2)))
