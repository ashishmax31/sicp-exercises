#lang racket/load
(current-directory (build-path (current-directory)))
(load "./ex3-59.scm")

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1)
                    (stream-car s2))
                 (add-streams (scale-stream (stream-car s2)
                                            (stream-cdr s1))
                              (mul-series s1
                                          (stream-cdr s2)))))

; Verify sin^2x + cos^2x = 1

(define res (add-streams (mul-series sine-series sine-series)
                         (mul-series cosine-series cosine-series))

; = 1 0 0 0 0 0 0 0 0 0