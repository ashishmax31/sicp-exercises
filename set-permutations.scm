#lang scheme
(define (set-permuations s)
    (if (null? s)
        (list null)
        (flatmap
                (lambda (x) (map
                                (lambda(y) (cons x y))
                                (set-permuations (remove x s))))
                s)))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate
                append
                '()
                (map proc seq)))

(define (filter op sequence) 
    (cond   ((null? sequence) '())
            ((op (car sequence)) (cons  (car sequence) 
                                        (filter op (cdr sequence))))
            (else (filter op (cdr sequence)))))
            


(define (remove item sequence)
    (filter (lambda(x)(not (equal? x item)))
            sequence))














