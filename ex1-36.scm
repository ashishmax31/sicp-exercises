#lang scheme


(define tolerance 0.000001)


(define (find-fixed-point func guess)
    (define(close-enough? a b)
        (< (abs (- a b)) tolerance))
    (define (try val)
        (newline)
        (display "trying value:")
        (display val)
        (let ((next (func val)))
             (if (close-enough? next val)
                    next
                    (try next))))
  (try guess)
  (newline))



(find-fixed-point (lambda (x)(/ (log 1000) (log x))) 2)


;   Interval dampening method, converges faster
(find-fixed-point (lambda (x) (* 0.5 (+ (/ (log 1000) (log x)) x))) 2)
