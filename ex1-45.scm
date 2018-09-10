#lang scheme

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average a b)
    (/ (+ a b) 2))

(define (repeated-apply f n)
    (if (= n 1)
        f
        (compose f (repeated-apply f (- n 1)))))

(define (compose f g)
    (lambda (x) (f (g x))))


(define (find-nth-root x)
    (find-fixed-point ((repeated-apply average-damp 2) (lambda (y) (/ x (* y y y)))) 1)) 



(define tolerance 0.00001)

(define (find-fixed-point func guess)
    (define(close-enough? a b)
        (< (abs (- a b)) tolerance))
    (define (try val)
        (let ((next (func val)))
             (if (close-enough? next val)
                    next
                    (try next))))
  (try guess)) 