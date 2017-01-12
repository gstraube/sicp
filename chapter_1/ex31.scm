(define (identity x) x)

(define (inc n) (+ n 1))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
