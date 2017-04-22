#lang racket

(require scheme/mpair)

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (mcar (mcar records)))
           (mcar records))
          (else (assoc key (mcdr records)))))
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable 
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (mcdr subtable))))
              (if record (mcdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable 
             (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record 
                   (assoc key-2 
                          (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! 
                   subtable
                   (mcons (mcons key-2 value)
                         (mcdr subtable)))))
            (set-mcdr! 
             local-table
             (mcons (mlist key-1
                         (mcons key-2 value))
                   (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))
