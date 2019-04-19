#lang racket/load
(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")


(define (stream-starting-from n)
    (cons-stream n
                 (stream-starting-from (+ n 1))))

(define integers (stream-starting-from 1))


(define (partial-sums original-stream)
    (define (calculate-partial-stream-sum stream)
        (if (stream-null? stream)
            the-empty-stream
            (cons-stream (stream-sum (stream-until (stream-car stream) original-stream))
                         (calculate-partial-stream-sum (stream-cdr stream)))))
                         
    (calculate-partial-stream-sum original-stream))

(define (stream-until item stream)
    (if (or (stream-null? stream) (= item (stream-car stream)))
        (cons-stream (stream-car stream) the-empty-stream)
        (cons-stream (stream-car stream)
                     (stream-until item (stream-cdr stream)))))

(define (stream-sum stream)
    (if (stream-null? stream)
        0
        (+ (stream-car stream)
           (stream-sum (stream-cdr stream)))))

(define n-int (stream-until 20 integers))

(define (stream-starting-from n)
    (cons-stream n
                 (stream-starting-from (+ n 1))))

(define integers (stream-starting-from 1))

; OR 

(define (partial-sums stream)
    (define result (cons-stream (stream-car stream)
                                (add-streams (stream-cdr stream) result)))
    result)