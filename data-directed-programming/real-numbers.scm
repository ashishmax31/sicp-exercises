(define (install-real-numbers)
    (define (raise-to-complex number)
        (make-from-real-imag number
                             0))
    (define (tag contents) (attach-tag 'real-number contents))
    (put 'make 'real-number (lambda (n) (tag n)))
    (put 'add '(real-number real-number) (lambda (a b) (tag (+ a b))))
    (put 'sub '(real-number real-number) (lambda (a b) (tag (- a b))))
    (put 'mul '(real-number real-number) (lambda (a b) (tag (* a b))))
    (put 'div '(real-number real-number) (lambda (a b) (tag (/ a b))))
    (put 'raise '(real-number) (lambda (real-num) (raise-to-complex real-num)))

    (display "Installed real numbers...")
    (newline))
