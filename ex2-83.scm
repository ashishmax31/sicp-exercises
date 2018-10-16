#lang racket/load
(load "data-directed-programming/setup.scm")

(define (raise item)
    (apply-generic 'raise item))
