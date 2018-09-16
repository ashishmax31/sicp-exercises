#lang scheme

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (x)(if (pair? x)
                                    (count-leaves x)
                                    1))
                      tree)))

