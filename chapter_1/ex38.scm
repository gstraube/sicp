(define (cont-frac-iter n d k)
  (define (calc i result)
    (if (= i 0)
      result
      (calc (- i 1) (/ (n i) (+ (d i) result)))))
  (calc k (/ (n k) (d k))))

(define (tan-cf x k)
  (cont-frac-iter
   (lambda (i) (if (= i 1) x (- (expt x 2.0))))
   (lambda (i) (+ i (- i 1)))
   k))
