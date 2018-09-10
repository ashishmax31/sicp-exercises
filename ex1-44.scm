#lang scheme

(define dx 0.00001)

(define (smooth func)
    (lambda (x) (average (func (+ x dx)) (func x) (func (- x dx)))))

(define (repeated-apply f n)
    (if (= n 1)
        f
        (compose f (repeated-apply f (- n 1)))))

(define (compose f g)
    (lambda (x) (f (g x))))


(define (average a b c)
    (/ (+ a b c) 3))



((smooth square) 10)