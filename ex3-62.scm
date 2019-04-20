#lang scheme


(define (div-series s1 s2)
    (if (zero? (stream-car s2))
        (error "Denominator constant term cant be zero!")
        (mul-series s1 (invert-unit-series s2))))
