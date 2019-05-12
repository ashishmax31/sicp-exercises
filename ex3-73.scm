#lang racket/load
(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")

(define (integral integrand intial-value dt)
	(define int (cons-stream intial-value
							 (add-streams (scale-stream dt integrand)
							 			  int)))
    int)

(define (RC r c dt)
	(define (voltage-stream i-stream initial-voltage)
			(add-streams (scale-stream r i-stream)
						 (integral (scale-stream (/ 1 c) i-stream) initial-voltage dt)))

	voltage-stream)

(define rc1 (RC 5 1 0.5))
(define v (rc1 (scale-stream (/ 1 100) integers) 0.2))