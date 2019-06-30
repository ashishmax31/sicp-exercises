(define (enclosing-env env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! variable value frame)
  (set-car! frame (cons variable
                        (frame-variables frame)))
  (set-cdr! frame (cons value
                        (frame-values frame))))

(define (extend-environment vars values base-env)
  (if (equal? (length values) (length vars))
      (cons (make-frame vars values)
            base-env)
      (if (> (length vars) (length values))
          (error "Number of variables greater than number of values")
          (error "Number of values greater than number of variables"))))

(define (lookup-variable-value variable env)
  (define (env-lookup vars values)
    (cond ((null? vars) (lookup-variable-value variable
                                               (enclosing-env env)))
          ((eq? variable (car vars)) (car values))
          (else (env-lookup (cdr vars) (cdr values)))))
  
  (if (equal? env the-empty-environment)
    (error "Unbound variable" variable)
    (let ((current-frame (car env)))
      (env-lookup (frame-variables current-frame)
                  (frame-values current-frame)))))

(define (set-variable-value! variable value env)
  (define (env-lookup vars values)
    (cond ((null? vars) (set-variable-value! variable
                                             (enclosing-env env)))
          ((eq? variable (car vars)) (set-car! values value))
          (else (env-lookup (cdr vars) (cdr values)))))
  
  (if (equal? env the-empty-environment)
    (error "Unbound variable" variable)
    (let ((current-frame (first-frame env)))
      (env-lookup (frame-variables current-frame)
                  (frame-values current-frame)))))


(define (define-variable! variable value env)
  (let ((current-frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! variable
                                                value
                                                current-frame))
            ((eq? variable (car vars)) (set-car! vals value))
            (else (scan (cdr vars) (cdr vals)))))
    
    (scan (frame-variables current-frame)
          (frame-values current-frame))))