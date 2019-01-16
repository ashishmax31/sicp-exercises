; 9C = 5(F - 32)

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
    (let ((w (make-connector))
          (u (make-connector))
          (v (make-connector))
          (x (make-connector))
          (y (make-connector)))
          (constant w 9)
          (constant x 5)
          (constant y 32)
          (multiplier c w u)
          (multiplier v x u)
          (adder v y f)))

(celsius-fahrenheit-converter C F)
