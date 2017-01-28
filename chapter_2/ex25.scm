(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (extract z 2 3))

(define (cdr z)
  (extract z 3 2))

(define (extract z n1 n2)
  (if (= (remainder z n2) 0)
    (extract (/ z n2) n1 n2)
    (/ (log z) (log n1))))
