
(define (make-accumulator sum)
    (define (accumulate value)
        (set! sum (+ sum value))
        sum)
    accumulate)


(define A (make-accumulator 5))
(A 10)
(A 10)