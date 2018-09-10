#lang scheme

; (define us-coins (list 50 25 10 5 1))
(define us-coins (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc sum coins)
    (cond ((zero? sum) 1)
          ((or  (negative? sum)
                (no-more? coins))
           0)
           (else (+ (cc sum (except-first-coin coins))
                    (cc (- sum (get-first-denomination coins))
                        coins)))))

(define (no-more? coins)
    (null? coins))

(define (except-first-coin coins)
    (cdr coins))

(define (get-first-denomination coins)
    (car coins))