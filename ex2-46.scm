#lang scheme

(define (make-vect x y)
    (cons x y))

(define (x-cord-vect vector)
    (car vector))

(define (y-cord-vect vector)
    (cdr vector))

(define (add-vect v1 v2)
    (make-vect
        (+  (x-cord-vect v1)
            (x-cord-vect v2))
        (+  (y-cord-vect v1)
            (y-cord-vect v2))))

(define (sub-vect v1 v2)
    (make-vect
        (-  (x-cord-vect v1)
            (x-cord-vect v2))
        (-  (y-cord-vect v1)
            (y-cord-vect v2))))


(define (scale-vect a vector)
    (make-vect
        (* a (x-cord-vect vector)
        (* a (y-cord-vect vector)))))
