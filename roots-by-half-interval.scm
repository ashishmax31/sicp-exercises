#lang scheme

(define (find-roots func a b)
   (let ((f-a (func a))
         (f-b (func b)))
         (cond 
            ((and (negative? f-a) (positive? f-b)) (half-interval-roots func a b))
            ((and (positive? f-a) (negative? f-b)) (half-interval-roots func b a))
            (else (error "func(a) and func(b) should be postive and negative")))))
  

(define (half-interval-roots func a b)
    (let ((mid-point (/ (+ a b) 2)))
        (if (close-enough? a b) 
            mid-point    
            (let((test-val (func mid-point)))
                (cond
                ((positive? test-val) (half-interval-roots func a mid-point))
                ((negative? test-val) (half-interval-roots func mid-point b))
                ((zero? test-val) mid-point))))))


(define(close-enough? a b)
    (< (abs(- a b)) 0.001))



(find-roots sin 2 4)    
(find-roots (lambda (x) (- (* x x x) (* 2 x) 3)) 1 2)