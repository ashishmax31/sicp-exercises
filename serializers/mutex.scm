(define (make-mutex)
    (let ((cell (list #f)))
        (define (the-mutex message)
            (cond ((eq? message 'acquire )
                   (if (test-and-set! cell)
                       (the-mutex 'acquire )
                       'acquired ))
                  ((eq? message 'release )
                   (set-car! cell #f))))
        the-mutex))


; Actual implementation is machine specific 
(define (test-and-set! cell)
    #t)