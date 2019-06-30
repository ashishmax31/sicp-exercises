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
  ((analyze exp) env))


(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating-exp exp))
        ((quoted? exp) (analyze-quoted-expression exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if-expression? exp) (analyze-if exp))
        ((begin-sequence? exp) (analyze-sequence (begin-expressions exp)))
        ((cond-expression? exp) (analyze-if (cond->if (cond-expressions exp))))
        ((lambda? exp) (analyze-lambda exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression"))))

(define (analyze-self-evaluating-exp expression)
  (lambda (env) expression))

(define (analyze-quoted-expression expression)
  (let ((quoted-text (text-quoted expression)))
    (lambda (env) quoted-text)))

(define (analyze-variable variable)
  (lambda (env) (lookup-variable-value variable env)))

(define (analyze-assignment expression)
  (let ((variable (assignment-variable expression))
        (assignment-proc (analyze (assignment-body expression))))
    (lambda (env) (set-variable-value! variable
                                       (assignment-proc env)
                                       env)
      'ok)))

(define (analyze-definition expression)
  (if (procedure-definition? expression)
      (analyze-procedure-definition expression)
      (let ((variable (assignment-variable expression))
            (definition-body-proc (analyze (assignment-body expression))))
        (lambda (env) (define-variable! variable
                                        (definition-body-proc env)
                                        env)
          'ok))))

(define (analyze-procedure-definition expression)
  (let  ((name (definition-name expression))
         (parameters (definition-parameters expression))
         (body-proc (analyze-sequence (definition-body expression))))
    (lambda (env) (define-variable! name
                                    (make-procedure parameters body-proc env)
                                    env)
      'ok)))


(define (analyze-if expression)
  (let ((p-proc (analyze (if-predicate expression)))
        (c-proc (analyze (if-consequent expression)))
        (a-proc (analyze (if-alternate expression))))
    (lambda (env) (if (p-proc env)
                      (c-proc env)
                      (a-proc env)))))


(define (analyze-lambda expression)
  (let ((parameters (lambda-parameters expression))
        (body-proc (analyze-sequence (lambda-body expression))))
    (lambda (env) (make-procedure parameters body-proc env))))


(define (analyze-application expression)
  (define (execute-application proc args)
    (if (compound-procedure? proc)
        ((procedure-body proc) (extend-environment (procedure-parameters proc)
                                                   args
                                                   (procedure-env proc)))
        (apply-primitive-procedure proc args)))
  
  (let ((operator-proc (analyze (operator expression)))
        (operands-proc (map analyze (operands expression))))
    (lambda (env) (execute-application (operator-proc env)
                                       (map (lambda (operand-proc) (operand-proc env))
                                            operands-proc)))
    ))


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))

  (define (loop first-proc rest-proc)
    (if (null? rest-proc)
        first-proc
        (loop (sequentially first-proc (car rest-proc))
              (cdr rest-proc))))
  
  (let ((procs (map analyze exps)))
    (loop (car procs)
          (cdr procs))))


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


(define call-underlying-scheme-apply apply)

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedures-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


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
    (let ((output (eval input the-global-env)))
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









