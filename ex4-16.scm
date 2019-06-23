;(load "metacircular-evaluator/evaluator.scm")

(define (lookup-variable-value variable env)
  (define (scan vars vals)
    (cond ((null? vars) (lookup-variable-value variable
                                               (enclosing-env env)))
          ((eq? (car vars) variable) (dispatch-value (car vals)))
          (else (scan (cdr vars)
                      (cdr vals)))))
  
  (define (dispatch-value value)
    (if (eq? value '*unassigned*)
        (error "Attempt to read unassigned value")
        value))
  
  (if (equal? env the-empty-environment)
      (error "Unbound variable" variable)
      (let ((current-frame (first-frame env)))
        (scan (frame-variables current-frame)
              (frame-values current-frame))))
  )

(define (make-procedure parameters body env)
    (list 'procedure parameters (scan-out-defines body) env))


(define (contains-local-definition? procedure-body)
  (cond ((null? procedure-body) #f)
      	((definition? (car procedure-body)) #t)
        (else (contains-local-definition? (cdr procedure-body)))))


(define (local-definitions procedure-body)
  (cond ((null? procedure-body) '())
        ((definition? (car procedure-body)) (cons (car procedure-body)
                                                  (local-definitions (cdr procedure-body))))
        (else (local-definitions (cdr procedure-body)))))


(define (remaining-expressions procedure-body)
  (cond ((null? procedure-body) '())
        ((definition? (car procedure-body)) (remaining-expressions (cdr procedure-body)))
        (else (cons (car procedure-body)
                    (remaining-expressions (cdr procedure-body))))))


(define (scan-out-defines procedure-body)
  (if (contains-local-definition? procedure-body)
      (normal-definition->simultaneous-definition (local-definitions procedure-body)
                                                  (remaining-expressions procedure-body))
      procedure-body))

; If procedure defn then get the procedure name, else get the definition variable.
(define (definition-names defns)
  (map (lambda (defn) (if (procedure-definition? defn)
                          (definition-name defn)
                          (assignment-variable defn)))
       defns))


(define (definition-bodies defns)
  (map (lambda (defn)
         (if (procedure-definition? defn)
             (make-lambda (definition-parameters defn)
                          (definition-body defn))
             (assignment-body defn)))
       defns))


(define (make-let let-variables let-exps)
  (list (append (list 'let
        	    (make-let-variables let-variables))
            let-exps)))


(define (make-let-variables variables)
  (if (null? variables)
      '()
      (cons (list (car variables)
                  '''*unassigned)
            (make-let-variables (cdr variables)))))

(define (set-variable-exps variables bodies)
  (if (null? variables)
      '()
      (cons (list 'set!
                  (car variables)
                  (car bodies))
            (set-variable-exps (cdr variables)
                               (cdr bodies)))))


(define (normal-definition->simultaneous-definition local-defs rem-exps)
  (let ((variables (definition-names local-defs))
        (bodies (definition-bodies local-defs)))
    (make-let variables (append (set-variable-exps variables bodies)
                                rem-exps))))



;(define test
;  '((define x (+ 10 1))
;    (define y (+ 20 32))
;    (+ x y)))


;(define (test t)
;  (define (x n) (+ 10 n))
;  (define (y n) (+ 20 n))
;  (+ (x 10) (y t)))


;;(scan-out-defines test)
;(let ((x *unassigned*)
;      (y *unassigned*))
;  (set! x (lambda (n) (+ 10 n)))
;  (set! y (lambda (n) (+ 20 n)))
;  (+ (x 10) (y 20)))