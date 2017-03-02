#lang racket

(define proc-table (make-hash '()))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (install-make-sum)
  (define (addend s) (car s))
  (define (augend s) (cadr s))

  (hash-set! proc-table '(deriv +)
             (lambda (exp var)
               (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var)))))

(define (install-make-product)
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))

  (hash-set! proc-table '(deriv *)
             (lambda (exp var)
               (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))))))

(install-make-sum)
(install-make-product)

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