#lang scheme

(define (derivative expression var)
    (cond ((number? expression) 0)
          ((variable? expression) (if (same-variable? expression var) 
                                      1 
                                      0))
          ((exponentiation? expression) (make-product (make-product (exponent expression)
                                                                    (make-exponentiation (base expression)
                                                                                         (dec (exponent expression))))
                                                      (derivative (base expression) var)))
          ((is-sum? expression) (make-sum (derivative (first-exp expression) var)
                                          (derivative (second-exp expression) var)))
          ((is-product? expression) (make-sum (make-product (first-exp expression)
                                                            (derivative (second-exp expression) var))
                                              (make-product (second-exp expression)
                                                            (derivative (first-exp expression) var))))))
(define (variable? exp)
    (symbol? exp))

(define (same-variable? expr var)
    (and (symbol? expr) (eq? expr var)))

(define (first-exp exp)
    (cadr exp))

(define (second-exp exp)
    (cadr (cdr exp)))

(define (is-sum? expression)
    (and (pair? expression) (eq? (car expression) '+)))

(define (is-product? expression)
    (and (pair? expression) (eq? (car expression) '*)))

(define (make-sum exp1 exp2)
    (cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
          ((=number? exp1 0) exp2)
          ((=number? exp2 0) exp1)
          ((eq? exp1 exp2) (list '2 exp1))
          (else (list '+ exp1 exp2))))

(define (make-product exp1 exp2)
    (cond ((or (=number? exp1 0) (=number? exp2 0)) 0)
          ((=number? exp1 1) exp2)
          ((=number? exp2 1) exp1)
          ((and (number? exp1) (number? exp2)) (* exp1 exp2))
          ((eq? exp1 exp2) (list exp1 '^2))
          (else (list '* exp1 exp2))))

(define (=number? exp val)
    (and (number? exp) (equal? exp val)))


(define (exponentiation? expression)
    (and (pair? expression) (eq? (car expression) '**)))

(define (exponent expression)
    (cadr (cdr expression)))

(define (base expression)
    (cadr expression))

(define (make-exponentiation b exp)
    (cond ((zero? exp) 1)
          ((equal? exp 1) b)
          (else (list '** b exp))))

(define (dec item)
    (- item 1))