#lang scheme

(define true #t)
(define false #f)


(define (left-branch h-tree)
    (car h-tree))

(define (right-branch h-tree)
    (cadr h-tree))

(define (leaf? h-tree)
    (eq? (car h-tree) 'leaf))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (symbol-leaf tree)
    (cadr tree))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))


(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (weight tree)
    (if (leaf? tree)
        (leaf-weight tree)
        (cadddr tree)))

(define (leaf-weight tree)
    (caddr tree))

(define (generate-huffman-tree pairs)
    (successive-merge (make-ordered-pairs pairs)))

(define (adjoin-to-set item set)
    (cond ((null? set) (list item))
          ((equal? (car set) item) set)
          ((< (weight item) (weight (car set))) (cons item
                                                      set))
          (else (cons (car set)
                      (adjoin-to-set item (cdr set))))))


(define (make-ordered-pairs pairs)
    (if (null? pairs)
        '()
        (adjoin-to-set (car pairs)
                       (make-ordered-pairs (cdr pairs)))))

(define set (list '(leaf A 4) '(leaf B 3) '(leaf C 1) '(leaf D 2) '(leaf E 1)))

; First two item in the list will always be the ones with the least weights, so always merge them together.
(define (successive-merge ordered-pairs)
    (if (null? (cdr ordered-pairs))
        (car ordered-pairs)
        (successive-merge (adjoin-to-set (make-code-tree (first ordered-pairs)
                                                         (second ordered-pairs))
                                         (remaining ordered-pairs)))))

(define (first ordered-pairs)
    (car ordered-pairs))

(define (second ordered-pairs)
    (if (> (length ordered-pairs) 1)
        (cadr ordered-pairs)
        '()))

(define (remaining ordered-pairs)
    (cddr ordered-pairs))
