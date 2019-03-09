(define a (make-connector))
(define b (make-connector))
(define c (make-connector))


(define (averager a b c)
    (let ((x (make-connector))
          (y (make-connector)))
         (constant 2 x)
         (multiplier x c y)
         (adder a b y)))

(averager a b c)

(probe "a:" a)
(probe "b:" b)
(probe "c:" c)
