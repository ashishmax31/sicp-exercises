#lang scheme
; TO DO:
; How to implement begin for single expression with multiple sub expression, example: (assoc (quoted b)
;                                                                                            (quoted ((a 1) (b 1))))
; The above expression is a single expression. Begin shouldn't split them up.

(define (tagged-list? exp type) (and (pair? exp) (eq? (car exp) type)))
(define (first exps) (car exps))
(define (rest exps) (cdr exps))



(define (self-evaluating? exp) (cond ((number? exp) #t) ((string? exp) #t) (else #f)))
(define (quoted? exp) (tagged-list? exp 'quoted))
(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if-expression? exp) (tagged-list? exp 'if))
(define (begin-sequence? exp) (tagged-list? exp 'begin))
(define (cond-expression? exp) (tagged-list? exp 'cond))
(define (let-expression? exp) (tagged-list? exp 'let))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (application? exp) (pair? exp))
(define (procedure-definition? exp) (pair? (cadr exp)))
(define (last-exp? exp) (null? (cdr exp)))


;Variables
(define (variable-name exp) (car exp))

; Assignments and definitions
'(define (test xy) (asdsd) (dsadsa))


(define (assignment-variable exp) (cadr exp))
(define (assignment-body exp) (caddr exp))
(define (definition-name exp) (caadr exp))
(define (definition-parameters exp) (cdr (cadr exp)))
(define (definition-body exp) (cddr exp))

;If
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternate exp) (cadddr exp))

;Begin

(define (begin-expressions exp) (cdr exp))

;Cond expressions
(define (cond-expressions exp) (cdr exp))
(define (cond-predicate cond-exp) (car cond-exp))
(define (cond-actions cond-exp) (cdr cond-exp))



; Let expressions
(define (let-expressions exps) (cdr exps))
(define (let-parameters exps)
  (map car
       (first exps)))

(define (let-body exps) (cdr exps))
(define (let-values exps)
  (map cadr
       (first exps)))

; Lambda expressions

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; Procedure application

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; Quoted
(define (text-quoted exp)
    (cadr exp))



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
          ((let*-expression? exp) (eval (let*->nested-let (let*-variable-value-list (let*-expressions exp))
                                                          (let*-body (let*-expressions exp)))))
          ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
          ((application? exp) (apply-e (eval (operator exp) env)
                                       (list-of-values (operands exp) env)))
          (else (error "Unknown expression"))))

(define (look-up-variable-in-env name env)
    (cond ((null? env) (display "Unbound variable ") (display name) (newline)(error "exiting"))
          ((eq? (caar env) name) (cadr (car env)))
          (else (look-up-variable-in-env name (rest env)))))

(define (assignment-in-env name value env)
    (display "jere")
    (display name)
    (display value)
    (set! env (cons (list name value)
                    env))
    (display env))



(define (eval-assignment exp env)
    (assignment-in-env  (assignment-variable exp)
                        (eval (assignment-body exp) env)
                        env))

(define (eval-definition exp env)
    (if (procedure-definition? exp)
        (eval-procedure-definition exp env)
        (assignment-in-env (assignment-variable exp)
                           (eval (assignment-body exp) env)
                           env)))

(define (eval-procedure-definition exp env)
    (assignment-in-env (definition-name exp)
                       (make-lambda (definition-parameters exp)
                                    (definition-body exp)
                                    env)
                       env))

(define (eval-if-statement exp env)
    (cond ((eval (if-predicate exp) env) (eval (if-consequent exp) env))
          (else (eval (if-alternate exp) env))))


(define (make-begin-sequence exps)
    (if (> (length exps) 1)  
        (cons 'begin
              (expand-begin-sequence exps))
        (first exps)))

(define (expand-begin-sequence exps)
    (if (null? exps)
        '()
        (cons (first exps)
              (expand-begin-sequence (rest exps)))))

(define (make-if predicate consequent alternate)
    (list 'if predicate consequent alternate))

(define (cond->if exps)
    (if (and (last-exp? exps) (eq? (caar exps) 'else))
        (make-begin-sequence (cdar exps))
        (make-if (cond-predicate (first exps))
                 (make-begin-sequence (cond-actions (first exps)))
                 (cond->if (rest exps)))))

(define (let->combination let-exps env)
    (cons (make-lambda (let-parameters let-exps)
                       (let-body let-exps)
                       env)
          (list-of-values (let-values let-exps)
                          env)))

(define (eval-sequence exps env)
    (cond ((null? exps) '())
          ((last-exp? exps) (eval (first exps) env))
          (else (eval (first exps) env) (eval-sequence (rest exps) env))))

(define (list-of-values operands env)
    (if (null? operands)
        '()
        (cons (eval (first operands) env)
              (list-of-values (rest operands)
                              env))))

(define (primitive-procedure? procedure)
    #t)

;(define (apply-primitive-procedure))

(define (apply-e procedure arguments)
    (if (primitive-procedure? procedure)
        (apply procedure arguments)
        (apply procedure arguments)))

;(define (apply-compound-procedure procedure arguments)
;    (eval-sequence (procedure-body procedure)
;                   (extend-env (procedure-arguments procedure)
;                               arguments
;                               (procedure-env procedure))))

(define (make-lambda parameters body env)
    (cons 'lambda (cons parameters body)))

(define (make-procedure parameters body)
    '())

(define (let*-expression? expression) (tagged-list? 'let*))

(define (let*-expressions expression) (cdr expression))

(define (let*-variable-value-list let*-expressions)
  (first let*-expressions))

(define (let*-body let*-expressions)
  (cdr let*-expressions))

(define (make-let-expression let-variable-value-list body)
    (list 'let let-variable-value-list body))

;((x 3)
 ;(y (+ x 2))
 ;(z (+ x y)))
(define (let*->nested-let variable-value-exps body)
    (if (last-exp? variable-value-exps)
        (cons 'let
              (cons (list (first variable-value-exps))
                    body))
        (make-let-expression (list (first variable-value-exps))
                             (let*->nested-let (rest variable-value-exps) body))))


(define x '(((x 3) (y (+ x 2)) (z (+ x y))) (+ x y z)))

(define env (list (list 'x 10)
                  (list '+ +)
                  (list 'true #t)
                  (list 'false #f)
                  (list '= =)
                  (list 'car car)
                  (list 'assoc assoc)
                  (list 'cadr cadr)))

(define (run env)
    (lambda (command) (eval command env)))

(define runner (run env))
(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y)))
      (+ x y z))))







