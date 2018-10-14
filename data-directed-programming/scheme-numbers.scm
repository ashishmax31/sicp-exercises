(define (install-scheme-numbers)
    (define (tag contents)
        (attach-tag 'scheme-number contents))

    (put 'add '(scheme-number scheme-number) (lambda (a b) (tag (+ a b))))
    (put 'sub '(scheme-number scheme-number) (lambda (a b) (tag (- a b))))
    (put 'mul '(scheme-number scheme-number) (lambda (a b) (tag (* a b))))
    (put 'div '(scheme-number scheme-number) (lambda (a b) (tag (/ a b))))
    (put 'make 'scheme-number (lambda (num) (tag num)))

    (display "Installed scheme numbers...")
    (newline))
