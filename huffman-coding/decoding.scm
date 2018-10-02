#lang scheme

(define (decode bits tree)
    (define (decode-helper code current-branch)
        (if (null? code)
            '()
            (let ((next-branch (choose-branch (car code) current-branch)))
                 (if (leaf? next-branch)
                     (cons (symbol-leaf next-branch)
                           (decode-helper (cdr code) tree))
                     (decode-helper (cdr code) next-branch)))))
    (decode-helper bits tree))

(define (choose-branch bit tree)
    (if (zero? bit)
        (left-branch tree)
        (right-branch tree)))

(define (left-branch h-tree)
    (car h-tree))

(define (right-branch h-tree)
    (cadr htree))