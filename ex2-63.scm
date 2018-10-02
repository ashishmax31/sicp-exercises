#lang scheme

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (tree->list tree)
    (if (null? tree)
        '()
        (append (tree->list (left-branch tree))
                (cons (entry tree)
                      (tree->list (right-branch tree))))))


(define (tree->list1 tree)
    (define (tree-conv t result)
        (if (null? t)
            result
            (tree-conv (left-branch t)
                       (cons (entry t) (tree-conv (right-branch t)
                                                   result)))))
    (tree-conv tree '()))