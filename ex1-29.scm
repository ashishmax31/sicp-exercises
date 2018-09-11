(define (sum func a b next)
    (if (> a b)
         0
        (+ (func a) (sum func (next a) b next))
    )
)


(define (simpsons-finite-integral n a b func)
    (define (h a b n)
        (/ (- b a) n)
    )                
    (define(next a)
        (+ a h)
    )
    
    (* (/ (h) 3) (sum func a n next))
)




