(define (make-connector)
    (let ((informant false)
          (value false)
          (constraints '()))
         
        (define (set-value! newval setter)
            (cond ((not (has-value? me))
                   (set! value newval)
                   (set! informant setter)
                   (for-each-except setter
                                    inform-about-new-value
                                    constraints))
                   ((not (equal? newval value)) (error "Contradiction! Old value present!"))
                   (else  'ignored )))
        
        (define (forget-value! retractor)
            (if (eq? retractor informant)
                (begin 
                    (set! informant false)
                    (set! value false)
                    (for-each-except retractor
                                     inform-about-no-value
                                     constraints))
                'ignored ))

        (define (connect new-constraint)
            (if (not (memq new-constraint constraints))
                (set! constraints (cons new-constraint constraints))
                'ignored )
            (if (has-value? me)
                (inform-about-new-value new-constraint)
                'ignored ))

        (define (me request)
            (cond ((eq? request 'has-value? ) (if informant #t #f))
                  ((eq? request 'get-value ) value)
                  ((eq? request 'set-value! ) set-value!)
                  ((eq? request 'forget-value! ) forget-value!)
                  ((eq? request 'connect ) connect)
                  (else (error "Unknown request!"))))

        me))
