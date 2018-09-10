
; Pass odd limits only

(define (product f a b next)
    (define (product-iter func a b next acc)
        (if (> a b)
            acc
            (product-iter func (next a) b next (* acc (func a)))))
    (product-iter f a b next 1))



(define (pi-by-4 b)
    (define(next x)
        (+ x 2))

    (define (f1 x)
        (square (/ 1 x)))

    (define (square x)
        (* x x))

    (define (f2 x)
        (cond ( (= x 2) x)
              (else (square x))))
    
    (* (product f1 3 b next) (product f2 2 b next) (+ b 1))
)



