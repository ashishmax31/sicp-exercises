; (define (inform-about-value constraint) (constraint 'i-have-a-value ))
; (define (inform-about-no-value constraint) (constraint 'i-lost-my-value ))

(define (set-value! connector value informant)
    ((connector 'set-value! ) value informant))

(define (forget-value! connector retractor)
    ((connector 'forget-value! ) retractor))

(define (has-value? connector)
    (connector 'has-value? ))

(define (get-value? connector)
    (connector 'get-value ))

(define (connect connector new-constraint)
    ((connector 'connect ) new-constraint))


(define (probe name connector)
    (define (print-value value)
        (newline)
        (display name)
        (display " = ")
        (display value))
    
    
    (define (process-new-value)
          (print-value (get-value connector)))
          
    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (begin (newline) (display "probe: ?")))
              (else  (error "Unknown request!"))))
              
    (connect connector me))