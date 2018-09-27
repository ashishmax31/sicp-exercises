#lang scheme

(define (make-segment start-vector end-vector)
    (cons start-vector end-vector))

(define (start-vector segment)
    (car segment))

(define (end-vector segment)
    (cdr segment))