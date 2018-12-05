(define (or-gate a1 a2 output)
    (define (or-action)
        (let ((new-output (logical-or (get-signal a1)
                                      (get-signal a2))))
             (after-delay or-gate-delay
                          (lambda () (set-signal! output new-output)))))

    (add-action! a1 or-action)
    (add-action! a2 or-action))


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


(define (logical-or a1 a2)
    (cond ((and (= a1 0) (= a2 0)) 0)
          ((or (= a1 1) (= a2 1)) 1)
          (else (error "Unsupported signal in the wire!"))))

(define (logical-and a1 a2)
    (cond ((or (= a1 0) (= a2 0)) 0)
          ((and (= a1 1) (= a2 1)) 1)
          (else (error "Unsupported signal in the wire!"))))