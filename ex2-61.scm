#lang scheme


(define (element-of-set? set item)
    (cond ((null? set) false)
          ((equal? item (car set)) true)
          ((< item (car set)) false)
          (else (element-of-set? (cdr set) item))))

(define (adjoin-set set item)
    (cond ((null? set) (cons item set))
          ((= (car set) item) set)
          ((< item (car set)) (cons item set))
          (else (cons (car set)
                      (adjoin-set (cdr set) item)))))