#lang scheme


(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(define (accumulate-n op initial seqs)
    (if (null? (car seqs))
        '()
        (cons   (accumulate op 
                            initial
                            (map   (lambda (x) (car x))
                                    seqs))
                (accumulate-n   op 
                                initial 
                                (map (lambda (x) (cdr x))
                                     seqs)))))



(accumulate-n + 0 (list (list 1 2 3) (list 3 4 5) (list 6 7 8) (list 9 10 11)))