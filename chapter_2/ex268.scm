#lang racket

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (encode-symbol symbol tree)
  (define (traverse-tree current-tree symbols-list)
    (cond ((and (leaf? current-tree) (eq? (symbol-leaf current-tree) symbol))
           symbols-list)
          ((memq symbol (symbols (left-branch current-tree)))
           (traverse-tree (left-branch current-tree) (append symbols-list (list '0))))
          ((memq symbol (symbols (right-branch current-tree)))
           (traverse-tree (right-branch current-tree) (append symbols-list (list '1))))
          (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))
  (traverse-tree tree '()))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
