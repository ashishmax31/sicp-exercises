#lang scheme

(define (enumerate-tree tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))


(define (enumerate-sequence low high)
    (if (> low high)
        '()
        (cons low (enumerate-sequence (+ low 1) high))))



(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) (cons (car sequence)
                                            (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate sequence) (cons (car sequence)
                                            (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

