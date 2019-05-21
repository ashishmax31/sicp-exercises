(load "metacircular-evaluator/evaluator.scm")

(define (let-expression? exp) (tagged-list? exp 'let))

; Let expressions
(define (let-expressions exps) (cdr exps))
(define (let-parameters exps)
  (map car
       (first exps)))

(define (let-body exps) (cdr exps))
(define (let-values exps) (map cadr
                               (first exps)))


(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((quoted? exp) (text-quoted exp))
          ((variable? exp) (look-up-variable-in-env exp env))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if-expression? exp) (eval-if-statement exp env))
          ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
          ((cond-expression? exp) (eval (cond->if (cond-expressions exp)) env))
          ((let-expression? exp) (eval (let->combination (let-expressions exp) env) env))
          ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
          ((application? exp) (apply-e (eval (operator exp) env)
                                       (list-of-values (operands exp) env)))
          (else (error "Unknown expression"))))


(define (let->combination let-exps env)
    (cons (make-lambda (let-parameters let-exps)
                       (let-body let-exps)
                       env)
          (list-of-values (let-values let-exps)
                          env)))