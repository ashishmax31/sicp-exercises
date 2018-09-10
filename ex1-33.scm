
(define (accumulate combiner null-value term a next b filter)
    (define (accumulate-iter combiner term a next b acc)
        (if (> a b)
            acc
            (accumulate-iter combiner term (next a) next b (combiner acc ( if (filter a) (term a) null-value)))))
    (accumulate-iter combiner term a next b null-value))



(define (next-item x)
    (+ x 1))

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


display((accumulate + 0 square 2 next-item 150000 prime?))
(display "\n")