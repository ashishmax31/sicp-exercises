#lang scheme

;Normal implementation
(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((quoted? exp) (text-quoted exp))
		  ((variable? exp) (look-up-variable-in-env exp env))
		  ((assignment? exp) (eval-assignment exp env))
		  ((if-expression? exp) (eval-if-assignment exp env))
		  ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
		  ((cond-expression? exp) (eval (cond->if exp) env))
		  ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
		  ((and-expression? exp) (eval-and-sequence (and-expressions exp) env))
		  ((or-expression? exp) (eval-or-sequence (or-expressions exp) env))
		  ((application? exp) (apply (eval (operator exp) env)
		  							 (list-of-values (operands exp) env)))
		  (else (error "Unknown expression"))))

; Using derived expressions
(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((quoted? exp) (text-quoted exp))
		  ((variable? exp) (look-up-variable-in-env exp env))
		  ((assignment? exp) (eval-assignment exp env))
		  ((if-expression? exp) (eval-if-assignment exp env))
		  ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
		  ((cond-expression? exp) (eval (cond->if exp) env))
		  ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
		  ((and-expression? exp) (eval (and->if (and-expressions exp)) env))
		  ((or-expression? exp) (eval-or-sequence (or-expressions exp) env))
		  ((application? exp) (apply (eval (operator exp) env)
		  							 (list-of-values (operands exp) env)))
		  (else (error "Unknown expression"))))



(define (tagged-list? exp type) (eq? type (car exp)))
(define (and-expression? exp) (tagged-list? exp 'and))
(define (or-expression? exp) (tagged-list? exp 'or))
(define (and-expressions exp) (cdr exp))
(define (or-expressions exp) (cdr exp))

(define (eval-and-sequence exps env)
	(cond ((null? exps) 'true)
		  ((last-exp? exps) (eval (car exps) env))
		  ((false? (eval (car exps) env)) 'false)
		  (else (eval-and-sequence (cdr exps) env))))


(define (eval-or-sequence exps env)
	(cond ((null? exps) 'false)
		  ((true? (eval (car exps) env)) (eval (car exps) env))
		  (else (eval-and-sequence (cdr exps) env))))


(define (and->if exps)
	(if (null? exps)
		'true
		(expand-and->if exps)))

(define (expand-and->if exps env)
	(if (last-exp? exps)
		(car exps)
		(make-if (car exps) (expand-and->if (cdr exps)) 'false)))

(define (or->if exps)
	(if (null? exps)
		'false
		(expand-or->if exps)))

(define (expand-or->if exps)
	(if (null? exps)
		'false
		(make-if (car exps) (car exps) (expand-or->if (cdr exps)))))
_
