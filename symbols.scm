#lang scheme

(define (memq sym items)
    (cond ( (null? items) false)
            ((eq? sym (car items)) items)
            (else (memq sym (cdr items)))))
