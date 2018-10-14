#lang racket/load
(load "data-directed-programming/setup.scm")


(define (same? items item)
    (if (null? items)
        #t
        (and (eq? (car items) item) (same? (cdr items) item))))

(define (present? item)
    (not (null? item)))

(define (apply-generic op . args)
    (let ((type-tags (map get-tag args)))
         (let ((proc (get op type-tags)))
              (if (not (null? proc))
                  (apply proc (map contents args))
                  (if (and (>= (length args) 2) (not (same? (type-tags) (car type-tags))))
                      (let ((type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args)))
                            (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                                  (cond ((present? t1->t2) (apply-generic op (t1->t2 a1) a2))
                                        ((present? t2->t2) (apply-generic op a1 (t2->t1 a2)))
                                        (else (error "Action dispatch error! Types not compatible")))))
                      (error "Action Dispatch error method not defined!"))))))