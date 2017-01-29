(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (extract-bound interval <))

(define (upper-bound interval)
  (extract-bound interval >))

(define (extract-bound interval comparator)
  (if (comparator (car interval) (cdr interval))
    (car interval)
    (cdr interval)))
