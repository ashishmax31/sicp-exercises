(define (sum func a b next)
  (if (> a b ) 0
      (+ (func a) (sum func (next a) b next))
   )
)


(define (definite-integral a b function dx)
  (define (next a)
    (+ a dx)
   )
  (* (sum function (+ a (/ dx 2)) b next) dx)
 )
  
(define (cube x)
   (* x x x)
)




