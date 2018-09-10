#lang scheme


(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (car i))

(define (lower-bound i)
  (cdr i))


(define (add-interval x y)
  (make-interval 
    (+ (upper-bound x) (upper-bound y))
    (+ (lower-bound x) (lower-bound y))))

(define (subtract-interval x y)
  (make-interval 
    (- (upper-bound x) (upper-bound y))
    (- (lower-bound x) (lower-bound y))))

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
  (mul-interval x
    (make-interval
      (/ 1 (upper-bound y))
      (/ 1 (lower-bound y)))))

  


;(mul-interval (make-interval 10 12) (make-interval 2 3))

(define (find-width x)
        (/ (- (upper-bound x) 
              (lower-bound x))
           2))


(define (check-addition-equivalance)
  (lambda (x y)
            (= (+ (find-width x)
                  (find-width y))
               (find-width (add-interval x y))))) 

(define (check-subtraction-equivalance)
    (lambda (x y)
            (=  (- (find-width x)
                   (find-width y))
                (find-width (subtract-interval x y)))))

; False 
(define (check-mul-equivalance)
    (lambda (x y)
            (= (* (find-width x)
                (find-width y))
                (find-width (mul-interval x y)))))

; False
(define (check-division-equivalance)
    (lambda (x y)
            (= (/ (find-width x)
                (find-width y))
                (find-width (divide-interval x y)))))