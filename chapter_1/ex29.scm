(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (g k) (+ a (* k h)))
  (define (y k) (f (g k)))
  (define (next x) (+ x (* 2.0 h)))
  (* (/ h 3.0)
     (+ (y 0)
	(* 4.0 (sum f (g 1) next (* (- n 1) h)))
	(* 2.0 (sum f (g 2) next (* (- n 2) h)))
	(y n))))
