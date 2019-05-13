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



(define (make-begin-sequence exps)
	(cons 'begin
		  (expand-begin-sequence exps)))

(define (expand-begin-sequence exps)
	(if (null? exps)
		'()
		(cons (car exps)
		   	  (expand-begin-sequence (cdr exps)))))

(define (cond->if exps)
	(if (and (last-exp? exps) (eq? (caar exp) 'else))
		(make-begin-sequence (cadr exps))
		(make-if (cond-predicate (car exps))
				 ((make-begin-sequence (cond-actions)) (cond-predicate (car exps)))
				 (cond->if (cdr exps)))))