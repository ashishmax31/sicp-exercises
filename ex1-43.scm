#lang scheme

(define (repeated-apply func n)
    (if (= n 1)
        f
        (compose f (repeated-apply f (- n 1)))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (square x)
    (* x x))

