(define (gcd a b)
    (if (zero? b)
            a
            (gcd b (remainder a b))
    )
)