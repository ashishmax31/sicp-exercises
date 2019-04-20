#lang racket/load

(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")

(define (inverse-stream stream) (if (stream-null? stream)
                                    the-empty-stream
                                    (cons-stream (/ 1 (stream-car stream))
                                                 (inverse-stream (stream-cdr stream)))))



(define (mul-streams a b)
    (s-map * a b))

(define (integrate-series stream)
    (mul-streams (inverse-stream integers) stream))

(define exp-series (cons-stream 1
                                (integrate-series exp-series)))


(define cosine-series (cons-stream 1
                                   (integrate-series (scale-stream -1 sine-series))))

(define sine-series (cons-stream 0
                                 (integrate-series cosine-series)))

