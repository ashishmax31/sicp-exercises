#lang scheme


(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-point s)
  (car s))

(define (end-point s)
  (cdr s))

(define (mid-point-segment segment)
  (make-point 
    (average (x-point (start-point segment))
             (x-point (end-point segment)))
    (average (y-point (start-point segment))
             (y-point (end-point segment)))))

(define (average a b)
  (/ (+ a b) 2))