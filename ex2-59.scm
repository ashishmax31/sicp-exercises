#lang scheme


(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? set2 (car set1))
                                            (cons (car set1)
                                                  (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))


(define (element-of-set? set item)
    (cond ((null? set) false)
          ((equal? (car set) item) true)
          (else (element-of-set? (cdr set) item))))

(define (adjoin-set set item)
    (if (element-of-set? set item)
        set
        (cons item set)))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((not (element-of-set? set2 (car set1))) (cons (car set1)
                                                         (union-set (cdr set1) set2)))
          (else (union-set (cdr set1) set2))))
          