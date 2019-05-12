#lang scheme

(define *op-table* (make-hash))

(define (put type proc)
  (hash-set! *op-table* (list type) proc))

(define (get type)
  (hash-ref *op-table* (list type) '()))

(define (expression-type exp)
	(car expression))

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((quoted? exp) (text-quoted exp))
		  ((variable? exp) (look-up-variable-in-env exp env))
		  ((not (null? (get (expression-type exp)))) ((get (expression-type exp)) exp env))
		  ((application? exp) (apply (eval (operator exp) env)
		  							 (list-of-values (operands exp) env)))
		  (else (error "Unknown expression"))))
