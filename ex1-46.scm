#lang scheme

(define (iterative-improve good-enough? improve)
    (define (iter-imp guess)
            (if (good-enough? guess)
                guess
                (iter-imp (improve guess))))
    iter-imp)


(define tolerance 0.00001)

(define (square x)
    (* x x))

(define (average a b)
    (/ (+ a b) 2))


(define (sqrt x)
    (iterative-improve (lambda (guess) (< (abs (- (square guess) x)) tolerance)) 
                       (lambda (guess) (average guess (/ x guess)))))

(define (fixed-point f)
        (iterative-improve 
            (lambda (guess) (< (abs (- (f guess) guess)) tolerance))
            (lambda(guess) (f guess))))






