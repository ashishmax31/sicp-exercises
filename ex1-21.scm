; “Use the smallest-divisor procedure to find the smallest divisor of each of the 
; following numbers: 199, 1999, 19999.”


(define (find-smallest-divisor num)
    (define (find-smallest-divisor-iter num smallest-divisor)
        (cond ((> (square smallest-divisor) num) num)
              ((zero? (remainder num  smallest-divisor)) smallest-divisor)
              (else (find-smallest-divisor-iter num (next smallest-divisor)))
        )
    )
    (find-smallest-divisor-iter num 2)
)

(define (square x)
    (* x x)
)


(define (next num)
    (if (= num 2)
        3
        (+ num 2)
    )
)


(display "find-smallest-divisor: 199 => \n")
(display (find-smallest-divisor 199))
(display "\n")

(display "find-smallest-divisor: 1999 =>\n")
(display (find-smallest-divisor 1999))
(display "\n")

(display "find-smallest-divisor: 19999 =>\n")
(display (find-smallest-divisor 19999))
(display "\n")