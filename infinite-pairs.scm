#lang racket/load

(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (pairs s1 s2) 
    (interleave 
                (s-map (lambda (x) (list (stream-car s1) x))
                       s2)
                (pairs (stream-cdr s1) (stream-cdr s2))))

