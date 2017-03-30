#lang racket

(define (call-the-cops)
  (error "Calling the cops"))

(define (make-account balance password)
  (define incorrect-tries 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  (define (check-password provided-password method)
    (if (eq? password provided-password)
        (begin
          (set! incorrect-tries 0)
          (dispatch method))
        (if (< incorrect-tries 3)
            (begin
              (set! incorrect-tries (+ incorrect-tries 1))
              (error "Incorrect password"))
            (call-the-cops))))
  check-password)

