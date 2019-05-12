#lang scheme

(define (integral delayed-integrand initial-value dt)
	(cons-stream initial-value
				 (let ((integrand (force delayed-integrand)))
				 	  (integral (delay (stream-cdr integrand))
				 	  			(+ (* (stream-car integrand) dt)
				 	  			   initial-value)
				 	  			dt))))
