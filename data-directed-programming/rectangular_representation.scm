(define (install-rectangular-complex-numbers)

    (define (make-from-real-imag x y)
        (cons x y))
    
    (define (make-from-magnitude-angle r A)
        (cons (* r (cos A))
              (* r (sin A))))

    (define (real-part z)
        (car z))
    
    (define (imaginary-part z)
        (cdr z))

    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imaginary-part z)))))

    (define (angle z)
        (atan (imaginary-part z)
              (real-part z)))

    (define (tag contents)
        (attach-tag 'rectangular contents))

    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imaginary-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
                              (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-magnitude-angle 'rectangular
                                    (lambda (r A) (tag (make-from-magnitude-angle r A))))

    (display "Installed rectangular representation...")
    (newline))

    