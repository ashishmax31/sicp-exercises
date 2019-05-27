(load "metacircular-evaluator/environment.scm")

(define (make-unbound! variable env)
  (define (env-loop vars values)
    (cond ((null? vars) (make-unbound variable
                                      (enclosing-env env)))
          ((eq? variable (car vars)) (begin (set! vars (cdr vars))
                                            (set! values (cdr values))))
          (else (env-loop (cdr vars) (cdr values)))))
  
  (if (equal? env the-empty-environment)
      (error "Unbound variable!" variable)
      (let ((current-frame (first-frame env)))
        (env-loop (frame-variables current-frame)
                  (frame-values current-frame)))))

