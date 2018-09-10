#lang scheme

(define (map proc items)
    (if (null? items)
        (quote())
        (cons 
            (proc (car items)) 
            (map proc (cdr items)))))