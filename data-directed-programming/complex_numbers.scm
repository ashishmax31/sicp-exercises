(define (install-complex-numbers)
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular ) x y))

    (define (make-from-mag-angle r A)
        ((get 'make-from-magnitude-angle 'polar ) r A))

    (define (add-complex x1 x2)
        (make-from-real-imag (+ (real-part x1)
                                (real-part x2))
                             (+ (imag-part x1)
                                (imag-part x2))))


    (define (sub-complex x1 x2)
        (make-from-real-imag (- (real-part x1)
                                (real-part x2))
                             (- (imag-part x1)
                                (imag-part x2))))


    (define (divide-complex x1 x2)
        (make-from-mag-angle (/ (magnitude x1)
                                (magnitude x2))
                             (- (angle x1)
                                (angle x2))))

    (define (mul-complex x1 x2)
        (make-from-mag-angle (* (magnitude x1)
                                (magnitude x2))
                             (+ (angle x1)
                                (angle x2))))

    (define (add-3-complex x1 x2 x3)
        (make-from-real-imag (+ (real-part x1)
                                (real-part x2)
                                (real-part x3))
                             (+ (imag-part x1)
                                (imag-part x2)
                                (imag-part x3))))

    (define (tag contents)
        (attach-tag 'complex contents))

    (put 'add '(complex complex)
               (lambda (z1 z2) (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
               (lambda (z1 z2) (tag (sub-complex z1 z2))))

    (put 'div '(complex complex)
               (lambda (z1 z2) (tag (divide-complex z1 z2))))

    (put 'mul '(complex complex)
               (lambda (z1 z2) (tag (mul-complex z1 z2))))

    (put 'add-3 '(complex complex complex)
                (lambda(z1 z2 z3) (tag (add-3-complex z1 z2 z3))))


    (put 'make-from-real-imag 'complex
                              (lambda (x y) (tag (make-from-real-imag x y))))

    (put 'make-from-magnitude-angle 'complex
                                    (lambda (r A) (tag (make-from-mag-angle r A))))


    (put 'raise '(complex) (lambda(c) '()))

    (display "Installed complex package...")
    (newline))
