#lang racket/load
(load "data-directed-programming/setup.scm")

(define (apply-generic op . args)
    (let ((type-tags (map get-tag args)))
         (let ((proc (get op type-tags)))
              (if (not (null? proc))
                  (apply proc (map contents args))
                  (if (and (>= (length args) 2) (not (same? type-tags (car type-tags))))
                      (let ((payload (map contents args))
                            (possible-transformations (map (lambda (to) (transform to type-tags))
                                                           type-tags)))
                           (let ((available-transformations (filter all? possible-transformations)))
                                (if (null? available-transformations)
                                    (error "ActionDispatch error cant coerce types.")
                                    (let ((selected-transformations (map (lambda (item) (cdr item))
                                                                         (flatten-result available-transformations)))
                                          (transform-to (car (car available-transformations))))
                                            (let ((transformed-args (apply-transformations selected-transformations
                                                                                           args)))
                                                  (apply apply-generic op transformed-args))))))
                      (error "ActionDispatch error cant coerce types."))))))

