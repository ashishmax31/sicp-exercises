(define (square x)
    (* x x )
)

(define (max x y z)
        
            (if(>= x y))
            (if(>= y z))
            (x y)
            (x z)
            (if (>= x z))
            (y x)
            (y z)
        
)

(define (max-s x y z)
    (if (>= x y)
        (if (>= y z)
            (+ (square x) (square y))
            (+ (square x) (square z))
        (if (>= x z)
        (+ (square y) (square x))
        (+   (square y) (square z))))))

(define (sum_of_squares x y z)
)



(define (factorial n)
  (if(= n 1)
     1
     (* n (factorial (- n 1)))))

(define (new-factorial n)
  (fact-iter 1 1 n))



(define (fact-iter product count max)
 (if(> count max)
    product
    (fact-iter (* product count)
                 (+ count 1)
                 max)))