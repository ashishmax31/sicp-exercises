(define (or-gate a1 a2 output)
    (let ((a3 (make-wire))
          (a4 (make-wire))
          (a5 (make-wire))
          (a6 (make-wire))
          (a7 (make-wire)))
         (and-gate a1 a2 a3)
         (and-gate a1 a2 a4)
         (not-gate a3 a5)
         (not-gate a4 a6)
         (and-gate a5 a6 a7)
         (not-gate a7 output)))

(define (and-gate a1 a2 output)
    (define (and-action)
        (let ((new-output (logical-and (get-signal a1)
                                       (get-signal a2))))
             (after-delay and-gate-delay
                          (lambda () (set-signal! output new-output)))))

    (add-action! a1 and-action)
    (add-action! a2 and-action))


(define (not-gate a output)
    (define (invert-input)
        (let ((new-output (logical-not a)))
             (after-delay not-gate-delay
                          (lambda () (set-signal! output new-output)))))
    (add-action! a invert-input))


(define (logical-not a)
    (cond ((= a 0) 1)
          ((= a 1) 0)
          (else (error "Unsupported signal in the wire!"))))