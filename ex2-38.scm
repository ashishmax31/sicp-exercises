#lang scheme

(define (fold-left op initial sequence)
    (define (result-iter result rest)
        (if (null? rest)
            result
            (result-iter (op result (car rest))
                         (cdr rest))))
    (result-iter initial sequence))

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))


(fold-left / 1 (list 1 2 3 4))  ; 1/24
(fold-right / 1 (list 1 2 3 4)) ; 3/8

; func (fold-right op initial sequence) == func (fold-left op initial sequence)
; If and only if the `op` given satisfies the commutative property.
