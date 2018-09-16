(define (flatmap proc seq)
    (accumulate
                append
                '()
                (map proc seq)))