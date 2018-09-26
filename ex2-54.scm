#lang scheme

(define (equal? a b)
    (cond ((and (null? a) (null? b)) true)
          ((and (list? a) (list? b)) (and (equal?
                                                (car a)
                                                (car b))
                                          (equal?
                                                (cdr a)
                                                (cdr b))))
          ((eq? a b) true)
          (else false)))
