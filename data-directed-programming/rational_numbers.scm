(define (install-rational-numbers)

    (define (tag contents) (attach-tag 'rational contents))

    (define (add-rat x y)
        (make-rat (+ (* (numer x)
                        (denom y))
                    (* (numer y)
                        (denom x)))
                (*
                    (denom x)
                    (denom y))))

    (define (sub-rat x y)
        (make-rat (- (* (numer x)
                        (denom y))
                    (* (numer y)
                        (denom x)))
                (*
                    (denom x)
                    (denom y))))

    (define (mul-rat x y)
        (make-rat (* (numer x)
                    (numer y))
                (* (denom x)
                    (denom y))))

    (define (divide-rat x y)
        (make-rat (* (numer x)
                    (denom y))
                (* (denom y)
                    (numer y))))

    (define (make-rat x y)
        (let ((gcd (gcd x y)))
                (tag (cons (/ x gcd) (/ y gcd)))))

    (define (numer x)
        (car x))

    (define (denom x)
        (cdr x))

    (define (print-rat x)
        (newline)
        (display (numer x))
        (display "/")
        (display (denom x)))

    (define (gcd a b)
        (if (= b 0)
            a
            (gcd b (remainder a b))))


    (put 'make-rat 'rational (lambda (x y) (make-rat x y)))
    (put 'add '(rational rational) add-rat)
    (put 'sub '(rational rational) sub-rat)
    (put 'mul '(rational rational) mul-rat)
    (put 'div '(rational rational) divide-rat)

    (put-coercion 'rational 'complex (lambda (r) (make-from-real-imag (/ (car (contents r))
                                                                         (cdr (contents r)))
                                                                      0 )))

    (display "Installed rational numbers...")
    (newline))
