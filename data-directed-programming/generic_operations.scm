(define (real-part z)
    (apply-generic 'real-part z))

(define (imag-part z)
    (apply-generic 'imag-part z))

(define (magnitude z)
    (apply-generic 'magnitude z))

(define (angle z)
    (apply-generic 'angle z))

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'complex ) x y))

(define (make-from-mag-angle r A)
    ((get 'make-from-magnitude-angle 'complex ) r A))

(define (make-rational-number x y)
    ((get 'make-rat 'rational) x y))

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (add z1 z2)
    (apply-generic 'add z1 z2))

(define (div z1 z2)
    (apply-generic 'div z1 z2))

(define (sub z1 z2)
    (apply-generic 'sub z1 z2))

(define (mul z1 z2)
    (apply-generic 'mul z1 z2))