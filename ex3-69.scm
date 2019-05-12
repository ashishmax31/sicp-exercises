#lang racket/load 

(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")


(define (pairs s1 s2)
    (cons-stream (list (stream-car s1) (stream-car s2))
                (interleave 
                            (s-map (lambda (x) (list (stream-car s1) x))
                                   (stream-cdr s2))
                            (pairs (stream-cdr s1) (stream-cdr s2)))))

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (triples s1 s2 s3)
    (cons-stream (map stream-car (list s1 s2 s3))
                 (interleave (s-map (lambda (x) (append '(stream-car s1) x))
                                    (stream-cdr (pairs s2 s3)))
                             (triples (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))

(define x (triples integers integers integers))

(define (square x) (* x x))


(define pythagorean-triplets (s-filter (lambda (triplet) (= (square (caddr triplet)) (+ (square (car triplet)) (square (cadr triplet)))))
                                            x))