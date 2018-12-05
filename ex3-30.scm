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


(define (half-adder a b sum carry)
    (let ((a1 (make-wire))
          (a2 (make-wire)))
         (or-gate a b a1)
         (and-gate a b carry)
         (not-gate carry a2)
         (and-gate a2 a1 sum)))


(define (full-adder a b c-in sum c-out)
    (let ((a1 (make-wire))
          (a2 (make-wire))
          (a3 (make-wire)))
         (half-adder b c-in a1 a2)
         (half-adder a a1 sum a3)
         (or-gate a3 a2 c-out)))

(define (ripple-carry-adder a-lines b-lines sum-lines carry)
    (define initial-carry (make-wire))
    (set-signal! initial-carry 0)
    (if (equal? (length a-lines) (length b-lines))
        (ripple-carry-adder-helper (reverse a-lines) (reverse b-lines) carry (reverse sum-lines) initial-carry)
        (error "Expected same number of inputs in both the input lines!")))

(define (ripple-carry-adder-helper a-lines b-lines carry-out sum-lines previous-carry)
    (if (last-bit? a-lines)
        (full-adder (car a-lines) (car b-lines) previous-carry (car sum-lines) carry-out)
        (let ((carry-for-the-next-adder (make-wire)))
             (full-adder (car a-lines) (car b-lines) previous-carry (car sum-lines) carry-for-the-next-adder)
             (ripple-carry-adder-helper (cdr a-lines) (cdr b-lines) carry-out (cdr sum-lines) carry-for-the-next-adder))))


(define (last-bit? line)
    (equal? (length line)
            1))