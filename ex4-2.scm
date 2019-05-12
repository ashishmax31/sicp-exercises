#lang scheme

(define (application? exp)
	(pair? (car exp)))

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((quoted? exp) (text-quoted exp))
		  ((variable? exp) (look-up-variable-in-env exp env))
		  ((assignment? exp) (eval-assignment exp env))
		  ((if-expression? exp) (eval-if-assignment exp env))
		  ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
		  ((cond-expression? exp) (eval (cond->if exp) env))
		  ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
		  ((application? exp) (apply (eval (operator exp) env)
		  							 (list-of-values (operands exp) env)))
		  (else (error "Unknown expression"))))


;Moving up the check for application? in the cond-expression, would break the interpreter as other expressions with
;different meanings come up as symbols(Example define, if etc). So we would need a different way of identifying a procedure application.


;Following evaluator would work:

(define (application? exp)
	(eq? 'call (car exp)))

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		  ((application? exp) (apply (eval (operator exp) env)
		  							 (list-of-values (operands exp) env)))  
		  ((quoted? exp) (text-quoted exp))
		  ((variable? exp) (look-up-variable-in-env exp env))
		  ((assignment? exp) (eval-assignment exp env))
		  ((if-expression? exp) (eval-if-assignment exp env))
		  ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
		  ((cond-expression? exp) (eval (cond->if exp) env))
		  ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
		  (else (error "Unknown expression"))))
