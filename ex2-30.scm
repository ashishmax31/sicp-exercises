#lang scheme

(define (square x)
    (* x x))

(define (square-tree tree)
    (cond   ((null? tree) null)
            ((not (pair? tree)) (square tree))
            (else (cons (square-tree (car tree))
                        (square-tree (cdr tree))))))


(define (square-tree-1 tree)
    (map (lambda (x) (if (pair? x)
                         (square-tree-1 x)
                         (square x))) tree))
