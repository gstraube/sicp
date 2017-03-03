#lang racket

(define proc-table (make-hash '()))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (make-exponentiation base exponent)
  (list '** base exponent))

(define (install-sum-rule)
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (hash-set! proc-table '(deriv +)
             (lambda (exp var)
               (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var)))))

(define (install-product-rule)
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))

  (hash-set! proc-table '(deriv *)
             (lambda (exp var)
               (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))))

(define (install-power-rule)
  (define (base e) (car e))
  (define (exponent s) (cadr s))

  (hash-set! proc-table '(deriv **)
             (lambda (exp var)
               (make-product
                (make-product (exponent exp)
                              (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))))

(install-sum-rule)
(install-product-rule)
(install-power-rule)

(define (deriv exp var)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((hash-ref proc-table (list 'deriv (operator exp))) (operands exp)
                                            var))))
