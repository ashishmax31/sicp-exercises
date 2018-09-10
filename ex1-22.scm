(define (find-smallest-divisor num)
    (define (find-smallest-divisor-iter num smallest-divisor)
        (cond ((> (square smallest-divisor) num) num)
              ((zero? (remainder num  smallest-divisor)) smallest-divisor)
              (else (find-smallest-divisor-iter num (next smallest-divisor)))
        )
    )
    (find-smallest-divisor-iter num 2)
)

(define (next num)
    (if (= num 2)
        3
        (+ num 2)
    )
)

(define (square x)
    (* x x)
)

; If a number is prime, its smallest divisor should be itself.
(define (prime? n)
    (= (find-smallest-divisor n) n)
)

(define (timed-prime-test? n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-time (- (runtime) start-time) n)
        #f
    )
)

(define (report-time elapsed-time n)
    (newline)
    (display n)
    (display " Elapsed time: *** ")
    (display elapsed-time)
    (newline)
    #t
)

(define (search-for-primes larger-than)
    (define (find-primes-iter current found)
        (cond ( (= found 3) #t)
              ( (and(> current larger-than) (timed-prime-test? current) ) (find-primes-iter (+ current 1) (+ found 1)))
              (else (find-primes-iter (+ current 1) found))
        )
    )
    (find-primes-iter 0 0)
)

(search-for-primes 100000065)
(search-for-primes 100000023)


