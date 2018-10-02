#lang scheme

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (element-of-set? item tree)
    (cond ((null? tree) false)
          ((equal? item (entry tree)) true)
          ((< item (entry tree)) (element-of-set? item (left-branch tree)))
          ((> item (entry tree)) (element-of-set? item (right-branch tree)))))

(define (adjoin-set set item)
    (cond ((null? set) (make-tree item '() '()))
          ((= (entry set) item) set)
          ((< item (entry set)) (make-tree (entry set) 
                                           (adjoin-set (left-branch set) item)
                                           (right-branch set)))
          ((> item (entry set)) (make-tree (entry set)
                                           (left-branch set)
                                           (adjoin-set (right-branch set) item)))))
 