#lang scheme


(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (car i))

(define (lower-bound i)
  (cdr i))


(define (mul-interval x y)
  (let ( 
          (p1 (* (upper-bound x) (lower-bound y)))
          (p2 (* (upper-bound x) (upper-bound y)))
          (p3 (* (lower-bound x) (lower-bound y)))
          (p4 (* (lower-bound x) (upper-bound y))))
        (make-interval
          (max p1 p2 p3 p4)
          (min p1 p2 p3 p4))))


(define (divide-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y))
          0)
     (error "Interval spans accross zero")
     (mul-interval x
        (make-interval
        (/ 1 (upper-bound y))
        (/ 1 (lower-bound y))))))

  
