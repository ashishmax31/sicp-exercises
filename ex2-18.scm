#lang scheme

(define (reverse-list items)
    (if (null? items)
        items
        (append 
             (list (car items)))))


(define (reverse-list-2 items)
    (define (reverse-iter items acc)
        (if (null? items)
            acc
            (reverse-iter (cdr items) (cons (car items) acc))))
    (reverse-iter items null))
