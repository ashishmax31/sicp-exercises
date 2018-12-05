(load "./primitive-gates.scm")

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