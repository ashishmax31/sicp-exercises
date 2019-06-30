;#lang scheme

(load "environment.scm")
; TO DO:
; How to implement begin for single expression with multiple sub expression, example: (assoc (quoted b)
;                                                                                            (quoted ((a 1) (b 1))))
; The above expression is a single expression. Begin shouldn't split them up.

(define (tagged-list? exp type) (and (pair? exp) (eq? (car exp) type)))
(define (first exps) (car exps))
(define (rest exps) (cdr exps))



(define (self-evaluating? exp) (cond ((number? exp) #t) ((string? exp) #t) (else #f)))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))
(define (if-expression? exp) (tagged-list? exp 'if))
(define (begin-sequence? exp) (tagged-list? exp 'begin))
(define (cond-expression? exp) (tagged-list? exp 'cond))
(define (let-expression? exp) (tagged-list? exp 'let))
(define (letrec-expression? exp) (tagged-list? exp 'letrec))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (application? exp) (pair? exp))
(define (procedure-definition? exp) (pair? (cadr exp)))
(define (last-exp? exp) (null? (cdr exp)))
(define (primitive-procedure? procedure)
  (tagged-list? procedure 'primitive))

(define (compound-procedure? procedure)
  (tagged-list? procedure 'procedure))


;Variables
(define (variable-name exp) (car exp))

; Assignments and definitions
;'(define (test xy) (asdsd) (dsadsa))


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



;letrec expressions


(define (let-rec->let expr)
  (make-let-rec (let-rec-expressions expr)
                (let-rec-body expr)))


(define (make-let-rec let-exps let-body)
  (make-let (let-variables let-exps)
            (append (make-required-set-exps let-exps)
                    let-body)))


(define (make-let let-variables let-body)
  (append (list 'let let-variables)
          let-body))


(define (let-rec-expressions expr)
  (cadr expr))

(define (let-rec-body expr)
  (cddr expr))

(define (let-variables exps)
  (map (lambda (letrec-exp) (list (car letrec-exp) '''*unassigned*))
       exps))

(define (make-required-set-exps letrec-exps)
  (map (lambda (letrec-exp) (list 'set! (car letrec-exp) (cadr letrec-exp)))
       letrec-exps))


; Lambda expressions

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

; Procedure application

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; Quoted
(define (text-quoted exp)
    (cadr exp))

; Compound procedures

(define (procedure-parameters procedure)
  (cadr procedure))

(define (procedure-body procedure)
  (caddr procedure))

(define (procedure-env procedure)
  (cadddr procedure))

; Primitive procedures

(define (procedure-object primitive-procedure)
  (cadr primitive-procedure))



(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((quoted? exp) (text-quoted exp))
          ((variable? exp) (lookup-variable-value exp env))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if-expression? exp) (eval-if-statement exp env))
          ((begin-sequence? exp) (eval-sequence (begin-expressions exp) env))
          ((cond-expression? exp) (eval (cond->if (cond-expressions exp)) env))
          ((let-expression? exp) (eval (let->combination (let-expressions exp) env) env))
          ((let*-expression? exp) (eval (let*->nested-let (let*-variable-value-list (let*-expressions exp))
                                                          (let*-body (let*-expressions exp)))))
          ((letrec-expression? exp) (eval (let-rec->let exp) env))
          ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
          ((application? exp) (apply-e (actual-value (operator exp) env)
                                       (operands exp)
                                       env))
          (else (error "Unknown (map (lambda (arg" (a)))))



(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-body exp) env)
                         env))

(define (eval-definition exp env)
    (if (procedure-definition? exp)
        (eval-procedure-definition exp env)
        (define-variable! (assignment-variable exp)
                          (eval (assignment-body exp) env)
                          env))
    'ok)

(define (eval-procedure-definition exp env)
    (define-variable! (definition-name exp)
                      (eval (make-lambda (definition-parameters exp)
                                         (definition-body exp))
                            env)
                       env))

(define (eval-if-statement exp env)
    (cond ((actual-value (if-predicate exp) env) (eval (if-consequent exp) env))
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
                       (let-body let-exps))
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

; Difference 1 
(define (make-lambda parameters body)
    (cons 'lambda  (cons parameters body)))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (let*-expression? expression) (tagged-list? expression 'let*))

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


(define (apply-primitive-procedure procedure arguments)
  (call-underlying-scheme-apply (procedure-object procedure)
                                arguments))

(define (apply-e procedure arguments env)
    (if (primitive-procedure? procedure)
        (apply-primitive-procedure procedure (map (lambda (arg) (actual-value arg env))
                                                  arguments))
        (apply-compound-procedure procedure arguments env)))

(define (apply-compound-procedure procedure arguments env)
    (eval-sequence (procedure-body procedure)
                   (extend-environment (procedure-parameters procedure)
                                       (list-of-delayed-args arguments env)
                                       (procedure-env procedure))))


(define (list-of-delayed-args arguments env)
  (if (null? arguments)
      '()
      (cons (delay-it (car arguments) env)
            (list-of-delayed-args  (cdr arguments)
                                  env))))


(define (actual-value expr env)
  (force-it (eval expr env)))

(define (delay-it expr env)
  (list 'thunk expr env))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))


(define (thunk-exp thunk)
  (cadr thunk))

(define (thunk-env thunk)
  (caddr thunk))

(define call-underlying-scheme-apply apply)

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedures-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (thunk? expr)
  (tagged-list? expr 'thunk))


(define primitive-procedures 
            (list 
                  (list '+ +)
                  (list 'true #t)
                  (list 'false #f)
                  (list '= =)
                  (list 'car car)
                  (list 'assoc assoc)
                  (list 'cadr cadr)
                  (list 'display display)
                  (list 'newline newline)
                  (list 'cons cons)
                  (list 'cdr cdr)
                  (list 'null? null?)
                  (list '< <)
                  (list '- -)
                  (list '* *)
                  (list '/ /)))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedures-objects)
                                         the-empty-environment)))
    initial-env))

(define the-global-env (setup-environment))
(define input-prompt "M-eval input:  ")
(define output-prompt "M-eval output: ")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define (prompt-for-input string)
  (newline)
  (display string)
  (newline))

;; Output

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print obj)
  (if (compound-procedure? obj)
      (display (list 'compound-procedure
         (procedure-parameters obj)
         (procedure-body obj)))
      (display obj)))









