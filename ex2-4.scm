#lang scheme

(define (cons x y)
  (lambda(m) (m x y)))


(define (car z)
  (z (lambda(a b) a)))

(define (cdr z)
  (z (lambda(a b) b)))


