#lang scheme


(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rectangle topRight buttomLeft)
  (cons topRight buttomLeft))

(define (first-edge r)
  (car r))

(define (second-edge r)
  (cdr r))

(define (average a b)
  (/ (+ a b) 2))

(define (breadth r)
  (abs (- (x-point (first-edge r)) 
          (x-point (second-edge r)))))

(define (height r)
  (abs (- (y-point (first-edge r)) 
          (y-point (second-edge r)))))

(define (perimeter r)
  (* 2
    (+ (breadth r)
       (height r))))

(define (area r)
  (* 
    (breadth r)
    (height r)))
