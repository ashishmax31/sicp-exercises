#lang scheme

;  Such a beauty (teary eyed)
(define (adjoin-set set item)
    (cond ((null? set) (cons item set))
          ((= (car set) item) set)
          ((< item (car set)) (cons item set))
          (else (cons (car set)
                      (adjoin-set (cdr set) item)))))

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x1 (car set1))
                      (x2 (car set2)))
                     (cond ((= x1 x2) (cons x1
                                            (union-set (cdr set1) (cdr set2))))
                           ((< x1 x2) (cons x1
                                            (union-set (cdr set1)
                                                       set2)))
                           (else (cons x2
                                       (union-set set1
                                                  (cdr set2)))))))))