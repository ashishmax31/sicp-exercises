#lang scheme

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center-percentage center percentage)
  (let  ((p (* center (/ percentage 100))))
        (make-center-width center p)))

(define (percent interval)
  (* (/ (width interval)
        (center interval))
     100))

(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (car i))

(define (lower-bound i)
  (cdr i))