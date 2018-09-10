#lang scheme


(define dx 0.000001)

(define (derivative func)
    (lambda (x) (/ (- (func (+ x dx)) (func x)) dx)))

(define tolerance 0.000001)


(define (find-fixed-point func guess)
    (define(close-enough? a b)
        (< (abs (- a b)) tolerance))
    (define (try val)
        (let ((next (func val)))
                (if (close-enough? next val)
                    next
                    (try next))))
    (try guess))


(define (find-roots-newton function)
    (find-fixed-point
        (lambda (x) (- x (/ (function x) ((derivative function) x))))
        1))                 


; Function to find square root:
(define (sqrt x)
    (lambda (y)( - (/ x y) y)))
    
(find-roots-newton (sqrt 25)) ; Returns 5



; Abstract out to the general case:
; “Each method begins with a function and finds a fixed point of some transformation of the function. We can express this general idea itself as a procedure:

; (define (fixed-point-of-transform g transform guess)
;   (fixed-point (transform g) guess))”

(define (fixed-point-of-transform g transform guess)
    (find-fixed-point (transform g) guess))


(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average a b)
    (/ (+ a b) 2))
    

; Square root function:
(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-dampen 1))
