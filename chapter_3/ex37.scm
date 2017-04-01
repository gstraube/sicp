#lang racket

(define (make-account balance password)
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
          ((eq? m 'unlock-account) dispatch)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  (define (check-password provided-password request)
    (if (eq? password provided-password)
        (dispatch request)
        (error "Incorrect password")))
  check-password)

(define (make-joint account existing-password new-password)
  (let ((unlocked-account (account existing-password 'unlock-account)))
        (lambda (password request)
          (if (eq? password new-password)
              (lambda (amount)
                ((unlocked-account request) amount))
                (error "Incorrect password")))))

