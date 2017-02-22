#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum-all all-args)
  (define (collect-and-sum args numbers non-numbers)
    (cond ((null? args) (list numbers non-numbers))
          ((number? (car args)) (collect-and-sum (cdr args) (+ numbers (car args)) non-numbers))
          (else (collect-and-sum (cdr args) numbers (append non-numbers (list (car args)))))))
  (collect-and-sum all-args 0 '()))

(define (make-sum a1 . rest)
  (define result (make-sum-all (append (list a1) rest)))
  (if (null? (cadr result))
      (car result)
      (if (= (car result) 0)
          (if (= (length (cadr result)) 1)
              (caadr result)
              (append (list '+) (cadr result)))
          (append (list '+ (car result)) (cadr result)))))

(define (make-product-all all-args)
  (define (collect-and-mult args numbers non-numbers)
    (cond ((null? args) (list numbers non-numbers))
          ((=number? (car args) 0) (list 0 '()))
          ((number? (car args)) (collect-and-mult (cdr args) (* numbers (car args)) non-numbers))
          (else (collect-and-mult (cdr args) numbers (append non-numbers (list (car args)))))))
  (collect-and-mult all-args 1 '()))
 
(define (make-product m1 . rest)
  (define result (make-product-all (append (list m1) rest)))
  (if (null? (cadr result))
      (car result)
      (if (= (car result) 1)
          (if (= (length (cadr result)) 1)
              (caadr result)
              (append (list '*) (cadr result)))
          (append (list '* (car result)) (cadr result)))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
