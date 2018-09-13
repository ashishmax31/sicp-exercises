#lang scheme


(define (fold-left op initial sequence)
    (define (result-iter result rest)
        (if (null? rest)
            result
            (result-iter (op result (car rest))
                         (cdr rest))))
    (result-iter initial sequence))

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))

(define (reverse sequence)
    (fold-right (lambda(x y) (append y (list x)))
                '()
                sequence))


(define (reverse sequence)
    (fold-left  (lambda(x y) (append (list y) x))
                '()
                sequence))