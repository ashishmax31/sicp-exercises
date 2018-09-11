#lang scheme


(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(define (horner-eval x coefficients-seq)
    (accumulate (lambda (this-coeff higher-terms)
                        (+ this-coeff
                           (* x higher-terms)))
                0
                coefficients-seq))


