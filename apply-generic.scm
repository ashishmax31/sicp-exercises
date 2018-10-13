#lang scheme

(define (apply-generic op . args)
    (let ((tags (map get-tag args)))
        (let ((proc (get op tags)))
            (if proc
                (apply proc (map contents args)))
                (error "Action Dispatch error! no operator found for the type"))))