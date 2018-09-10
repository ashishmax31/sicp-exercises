#lang scheme

(define (tree-map proc tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (proc tree))
          (else (cons (tree-map proc (car tree))
                      (tree-map proc (cdr tree))))))



(define (square-tree tree)
    (tree-map (lambda (x)
                    (* x x))
              tree))
