#lang scheme

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (times-divisible x num)
  (define (times-iter dividend num res)
    (if (zero? (remainder dividend num))
        (times-iter (/ dividend num) num (+ res 1))
        res))
  (times-iter x num 0))

(define (car x)
  (times-divisible x 2))

(define (cdr x)
  (times-divisible x 3))