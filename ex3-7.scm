#lang scheme

(define (println str)
    (display str)
    (newline))

(define (make-account balance password)
    (define consecutive-incorrect-entries 0)
    (define (withdraw amount)
        (cond ((>= balance amount) (begin (set! balance (- balance amount))
                                          (clear-counters)
                                          balance))
              (else (begin (println "Insufficient funds")
                           (newline)))))

    (define (deposit amount)
        (set! balance (+ balance amount))
        (clear-counters)
        balance)

    (define (handle-incorrect-password)
        (set! consecutive-incorrect-entries (+ consecutive-incorrect-entries 1))
        (if (> consecutive-incorrect-entries 7)
            (call-the-cops)
            (begin (println "Incorrect password")
                   #f)))

    (define (password-wrong? pass)
        (not (eq? password pass)))

    (define (clear-counters)
        (set! consecutive-incorrect-entries 0))

    (define (call-the-cops)
        (println "Dailing 911...")
        (newline)
        #f)
    
    (define (dispatch pass action)
        (if (password-wrong? pass)
            (handle-incorrect-password)
            (cond ((eq? action 'withdraw ) withdraw)
                  ((eq? action 'deposit ) deposit)
                  ((eq? action 'pass-check? ) #t )
                  (else (error "Unknown command!")))))
    
    dispatch)
    
(define (make-joint base-acc base-pass new-pass)

    (define (dispatch pass action)
        (if (eq? pass new-pass)
            (base-acc base-pass action)
            (println "Password incorrect!")))

    (if (base-acc base-pass 'pass-check? )
        dispatch
        (println "Base account password incorrect" )))

(define peter-acc (make-account 1000 'test))
(define paul-acc (make-joint peter-acc 'test 'testnew))

