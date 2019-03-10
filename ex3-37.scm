(define (c+ a b)
    (define z (make-connector))
    (adder a b z)
    z)

(define (c* a b)
    (define z (make-connector))
    (multiplier a b z)
    z)

(define (c/ a b)
    (define z (make-connector))
    (divide a b z)
    z)

(define (cv val)
    (define z (make-connector))
    (constant val z)
    z)

(define (celsius-fahrenheit-coverter x)
    (c+ (c* x
            (c/ (cv 9) (cv 5)))
        (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-coverter C))
(probe "c:" C)
(probe "f:" C)
