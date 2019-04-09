#lang racket/load
(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")


(define (mul-streams s1 s2)
    (if (or (stream-null? s1) (stream-null? s2))
        the-empty-stream
        (cons-stream (* (stream-car s1)
                        (stream-car s2))
                     (mul-streams (stream-cdr s1)
                                  (stream-cdr s2)))))


(define (stream-starting-from n)
    (cons-stream n
                 (stream-starting-from (+ n 1))))

(define integers (stream-starting-from 1))


(define factorials 
    (cons-stream 1
                 (mul-streams integers
                              factorials)))