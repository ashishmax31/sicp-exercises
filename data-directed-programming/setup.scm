#lang racket/load

(load "utils.scm")
(load "polar_representation.scm")
(load "rectangular_representation.scm")
(load "generic_operations.scm")
(load "complex_numbers.scm")
(load "rational_numbers.scm")
(load "scheme-numbers.scm")

(install-rectangular-complex-numbers)
(install-polar-representation)
(install-complex-numbers)
(install-rational-numbers)
(install-scheme-numbers)

(define z1 (make-from-real-imag 3 4))
(define z2 (make-from-real-imag 3 -14))
(define r1 (make-rational-number 1 2))
(define r2 (make-rational-number 7 8))
(define n1 (make-scheme-number 5))
(define n2 (make-scheme-number 10))
