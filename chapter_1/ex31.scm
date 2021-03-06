(define (identity x) x)

(define (inc n) (+ n 1))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter identity 1 inc n))

(define (calc-pi n)
  (define (term x)
    (define y (* 2 x))
    (* (/ y (- y 1))
       (/ y (+ y 1))))
  (* (/ 8 3)
     (product-iter term 2.0 inc n)))
