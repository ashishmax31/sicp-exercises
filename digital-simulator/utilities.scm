(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures))
               (call-each (cdr procedures)))))

(define (get-signal wire)
    (wire 'get-signal ))

(define (set-signal! wire new-value)
    ((wire 'set-signal! ) new-value))

(define (add-action wire action)
    ((wire 'add-action! ) action))


(define (probe name wire)
    (add-action wire
                (lambda () (
                    (newline)
                    (display name)
                    (display " ")
                    (display (current-time the-agenda))
                    (display " new-value = ")
                    (display (get-signal wire))))))