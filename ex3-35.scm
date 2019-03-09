(define (squarer a b)

    (define (square a)
        (* a a))

    (define (process-new-value)
        (if (has-value? a)
            (set-value! b (square (get-value a)) me)
            (if (has-value? b)
                (if (< (get-value b) 0)
                    (error "Square root of a negative number!")
                    (set-value! a (sqrt (get-value b)) me))
                'ignored )))



    (define (forget-value)
        (forget-value! a me)
        (forget-value! b me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
    (connect a me)
    (connect b me))
