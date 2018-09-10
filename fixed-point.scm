#lang scheme


(define tolerance 0.000001)


(define (find-fixed-point func guess)
    (define(close-enough? a b)
        (< (abs (- a b)) tolerance))
    (define (try val)
        (let ((next (func val)))
             (if (close-enough? next val)
                    next
                    (try next))))
  (try guess))