(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(define (f-tail-rec n a b c)
  (if (= n 0)
    c
    (f-tail-rec
      (- n 1)
      (+ (* 3 c) (* 2 b) a)
      a
      b)))

(define (f-iter n) (f-tail-rec n 2 1 0))
