(load "./utilities.scm")
(load "./wires.scm")
(load "./primitive-gates.scm")
(load "./compound-circuits.scm")
(load "./agenda.scm")

(define the-agenda (make-agenda))

(define input1 (make-wire))
(define input2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))



;Delays
(define or-gate-delay 4)
(define and-gate-delay 3) 
(define not-gate-delay 2)

(probe 'sum-wire sum)
(probe 'carry-wire carry)

(half-adder input1 input2 sum carry)



