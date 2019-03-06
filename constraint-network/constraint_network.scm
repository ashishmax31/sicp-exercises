; 9C = 5(F - 32)
#lang racket/load

(load "./connector.scm")
(load "./constraints.scm")
(load "./utils.scm")

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
    (let ((w (make-connector))
          (u (make-connector))
          (v (make-connector))
          (x (make-connector))
          (y (make-connector)))
          (multiplier c w u)
          (multiplier v x u)
          (adder v y f)
          (constant 9 w)
          (constant 5 x)
          (constant 32 y)
          'ok ))

(celsius-fahrenheit-converter C F)
(probe "celsius" C)
(probe "Fahrenheit" F)
