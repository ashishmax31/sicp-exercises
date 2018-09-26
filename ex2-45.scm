#lang scheme

(define (split combiner-direction split-direction)
    (lambda (painter)
        (combiner-direction painter
                            (split-direction painter painter))))







