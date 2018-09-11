#lang scheme


(define (slow-mul a b)
  ( if (= b 0) 0
       (+ a (slow-mul a (- b 1)))))

(define (fast-mul a b)
  ( cond ((= b 1) b)
         ((even? b) (double (fast-mul a (halve b))))
         (else (+ a (fast-mul a (- b 1))))
         )
  )
  
(define (double x)
  (+ x x)
  )
(define (halve x)
  (/ x 2)
  )

(define (mul-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (mul-iter (double a) (halve b) acc))
        ((odd? b) (mul-iter a (- b 1) (+ a acc)))
        )
  )