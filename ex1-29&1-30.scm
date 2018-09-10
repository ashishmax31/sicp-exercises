; Iterative solution
(define (sum func a b next)
    (define (sum-iter func a b next acc)
        ( if (> a b)
                acc 
                (sum func (next a) b next (+ acc (func a)))
        )
    )
    (sum-iter func a b next 0)
)

(define (eval-simpsons-rule func a b n)

    (define (next a)
        (+ a 1)
    )
    
    (define (simpsons-term k)
        ( cond ((zero? k)(func (+ a (* k h))))
               ((= k n) (func (+ a (* k h))) )
               ((even? k) (* 2 (func (+ a (* k h)))))
               ((odd? k) (* 4 (func (+ a (* k h)))))
        )
    )

    (define h (/ (- b a) n))
    (* (/ h 3) (sum simpsons-term a n next))

)


(define (cube x)
    (* x x x)
)