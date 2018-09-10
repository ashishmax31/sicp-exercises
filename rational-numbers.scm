#lang scheme

(define (add-rat x y)
    (make-rat (+ (* 
            (numer x) 
            (denom y)) 
          (* 
            (numer y) 
            (denom x)))
        (*
          (denom x)
          (denom y))))

(define (sub-rat x y)
    (make-rat (- (* 
            (numer x) 
            (denom y)) 
          (* 
            (numer y) 
            (denom x)))
       (*
         (denom x)
         (denom y))))

(define (mul-rat x y)
    (make-rat (*
          (numer x)
          (numer y))
       (*
          (denom x)
          (denom y))))

(define (divide-rat x y)
    (make-rat
      (* (numer x)
         (denom y))
      (* (denom y)
         (numer y))))

(define (make-rat x y)
      (let (
              (gcd (gcd x y)))
            (cons (/ x gcd) (/ y gcd))))

(define (numer x)
      (car x))

(define (denom x)
      (cdr x))


(define (print-rat x)
      (newline)
      (display (numer x))
      (display "/")
      (display (denom x)))



(define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
        