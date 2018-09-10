#lang scheme

(define (square x)
    (* x x))

(define (square-list-1 items)
    (if (null? items)
        null
        (cons (square (car items))
              (square-list (cdr items)))))

(define (map proc items)
    (if (null? items)
        (quote())
        (cons 
            (proc (car items)) 
            (map proc (cdr items)))))
(define (square-list items)
    (map square items))