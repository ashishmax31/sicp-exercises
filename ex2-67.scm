#lang scheme



(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf leaf)
    (cadr leaf))

(define (weight-leaf leaf)
    (caddr leaf))


(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

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
    (cadr h-tree))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))



(decode sample-message sample-tree)
; (A D A B B C A)


