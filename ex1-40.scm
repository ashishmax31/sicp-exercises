(define (cubic a b c)
    (find-roots-newton (lambda (x) (+ (cube x) (* a (square x)) (* b x) c))))


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
        2))     

(define (cube x)
    (* x x x))

(define (square x)
    (* x x))