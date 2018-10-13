#lang scheme

; Put and get are not primitive operators, below given implementation are 
; copied from: https://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on#5499256
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (derivative exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'derivative (operator exp)) (operands exp)
                                                  var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? exp) (symbol? exp))
(define (same-variable? exp var) (eq? exp var))

(define (install-sum-derivative)
    (define (make-sum a b) (list '+ a b))
    (define (sum-derivative operands var)
        (make-sum (derivative (addent operands) var)
                  (derivative (augend operands) var)))
    (define (addent operands) (car operands))
    (define (augend operands) (cadr operands))
    (put 'derivative '+ sum-derivative))




(define (install-product-derivative)
    (define (make-product a b) (list '* a b))
    (define (make-sum a b) (list '+ a b))
    (define (product-derivative operands var)
        (make-sum (make-product (multiplier operands)
                                (derivative (multiplicant operands) var))
                  (make-product (multiplicant operands)
                                (derivative (multiplier operands) var))))
    
    
    (define (multiplier operands) (car operands))
    (define (multiplicant operands) (cadr operands))
    (put 'derivative '* product-derivative))

(define (install-exponentiation-derivative)
    (define (make-product a b) (list '* a b))
    (define (exponent-derivative expression var)
        (make-product (make-product (exponent expression)
                                    (make-exponentiation (base expression)
                                                         (dec (exponent expression))))
                      (derivative (base expression) var)))
    (define (make-exponentiation base exponent) (list '** base exponent))

    (define (exponent expression) (cadr expression))
    (define (base expression) (car expression))
    (define (dec num) (- num 1))
    (put 'derivative '** exponent-derivative))


(install-exponentiation-derivative)
(install-product-derivative)
(install-sum-derivative)
(derivative '(+ (** x 5) (* x (+ x 4))) 'x)