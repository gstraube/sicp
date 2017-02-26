#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (weight-leaf x) (caddr x))

(define (symbol-leaf x) (cadr x))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge ordered-pairs)
  (cond ((or (= (length ordered-pairs) 0) (= (length ordered-pairs) 1))
        (car ordered-pairs))
        (else
         (successive-merge (cons (make-code-tree (car ordered-pairs)
                                                 (cadr ordered-pairs))
                                 (cddr ordered-pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
