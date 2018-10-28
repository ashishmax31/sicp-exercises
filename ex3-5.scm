#lang scheme
(define (estimate-integral predicate x1 x2 y1 y2 num-trials)
    (define (estimate-integral-iter passed num-iter)
        (cond ((= num-iter num-trials) (/ passed num-trials))
              ((predicate (random-in-range x1 x2) (random-in-range y1 y2))
               (estimate-integral-iter (+ passed 1) (+ num-iter 1)))
              (else (estimate-integral-iter passed (+ num-iter 1)))))
    
    (/ (* 36 (estimate-integral-iter 0 0)) 9.0))
 

(define (in-unit-circle? x y)
    (<= (+ (square x) (square y)) 1))

(define (random-in-range low high)
    (let ((range (- high low)))
         (+ low (random range))))

(define (square a)
    (* a  a))

(define (estimate-pi predicate x1 x2 y1 y2 num-trials)
    (define (estimate-integral-iter passed num-iter)
            (cond ((= num-iter num-trials) (/ passed num-trials))
                ((predicate (random-in-range x1 x2) (random-in-range y1 y2))
                 (estimate-integral-iter (+ passed 1) (+ num-iter 1)))
                (else (estimate-integral-iter passed (+ num-iter 1)))))
    
    (let ((rect-area (* (- x2 x1) (- y2 y1))))
        (* rect-area (estimate-integral-iter 0 0))))

