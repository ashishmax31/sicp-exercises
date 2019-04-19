#lang racket/load

(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")

(define (inverse-stream stream) (cons-stream (/ 1 (stream-car stream))
                                             (inverse-stream (stream-cdr stream))))


(define (mul-streams a b)
	(stream-map * a b))

(define (integrate-series stream)
	(mul-streams (inverse-stream integers) stream))


