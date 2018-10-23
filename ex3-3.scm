(define (make-account balance password)
    (define consecutive-incorrect-entries 0)
    (define (withdraw amount)
        (cond ((>= balance amount) (begin (set! balance (- balance amount))
                                          (clear-counters)
                                          balance))
              (else (begin (display "Insufficient funds")
                           (newline)))))

    (define (deposit amount)
        (set! balance (+ balance amount))
        (clear-counters)
        balance)

    (define (handle-incorrect-password)
        (set! consecutive-incorrect-entries (+ consecutive-incorrect-entries 1))
        (if (> consecutive-incorrect-entries 7)
            (call-the-cops)
            (display "Incorrect password")))

    (define (password-wrong? pass)
        (not (eq? password pass)))

    (define (clear-counters)
        (set! consecutive-incorrect-entries 0))

    (define (call-the-cops)
        (display "Dailing 911...")
        (newline))
    
    (define (dispatch pass action)
        (if (password-wrong? pass)
            (handle-incorrect-password)
            (cond ((eq? action 'withdraw ) withdraw)
                  ((eq? action 'deposit ) deposit)
                  (else (error "Unknown command!")))))
    
    dispatch)
    
    