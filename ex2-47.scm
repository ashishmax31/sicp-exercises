#lang scheme

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (edge1-frame frame)
    (car (cdr frame)))

(define (edge2-frame frame)
    (if (null? (cdr frame))
        (car frame)
        (edge2-frame (cdr frame))))

(define (origin-frame frame)
    (car frame))

; For the second constructor 

(define (edge1-frame frame)
    (car (cdr frame)))

(define (edge2-frame frame)
    (cdr (cdr frame)))

(define (origin-frame frame)
    (car frame))

(define (make-vect a b)
    (cons a b))