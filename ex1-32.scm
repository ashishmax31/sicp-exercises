
; Linear recursive process
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))



; Iterative process
(define (accumulate combiner null-value term a next b)
    (define (accumulate-iter combiner term a next b acc)
        (if (> a b)
            acc
            (accumulate-iter combiner term (next a) next b (combiner acc (term a)))))
    (accumulate-iter combiner term a next b null-value))


(define (cube x)
    (* x x x))

(define (next-item x)
    (+ x 1))

; Sum of cubes from 1 to 10
(accumulate + 0 cube 1 next-item 10)

; Product of cubes from 1 to 10
(accumulate * 1 cube 1 next-item 10)




