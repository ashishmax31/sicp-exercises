(define (install-polar-representation)
    
    (define (make-from-mag-angle r A)
        (cons r A))

    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x)
                       (square y)))
              (atan y x)))

    (define (magnitude z)
        (car z))
    
    (define (angle z)
        (cdr z))

    (define (real-part z)
        (* (magnitude z)
           (cos (angle z))))

    (define (imaginary-part z)
        (* (magnitude z)
           (sin (angle z))))

    (define (tag contents)
        (attach-tag 'polar contents))

    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imaginary-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar 
                              (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-magnitude-angle 'polar
                                    (lambda (r A) (tag (make-from-mag-angle r A))))

    (display "Installed polar representation...")
    (newline))