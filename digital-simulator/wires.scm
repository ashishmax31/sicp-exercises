(load "./utilities.scm")

(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))

        (define (set-signal! value)
            (if (not (equal? value signal-value))
                (begin (set! signal-value value)
                       (call-each action-procedures))
                       'done))

        (define (add-action-procedure! procedure)
            (begin (set! action-procedures
                         (cons procedure
                               action-procedures))
                   (procedure)))
                   
        (define (dispatch message)
            (cond ((eq? message 'set-signal! ) set-signal!)
                  ((eq? message 'add-action! ) add-action-procedure!)
                  ((eq? message 'get-signal ) signal-value)
                  (else (error "Unknown action for WIRE!"))))
        dispatch))

