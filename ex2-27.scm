#lang scheme

(define (deep-reverse items)
    (cond ((null? items) null)
          (else (append (deep-reverse (cdr items)) (list (if (pair? (car items))
                                                        (deep-reverse (car items))
                                                        (car items)))))))

