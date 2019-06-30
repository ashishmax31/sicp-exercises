(load "improved-evaluator/evaluator.scm")


(define (analyze-let-exp expression)
  (let ((variables (let-parameters expression))
        (let-values-proc (map analyze (let-values expression)))
        (let-body-proc (analyze-sequence (let-body expression))))
    (lambda (env)
      (let-body-proc (extend-environment variables
                                         (map (lambda (proc) (proc env))
                                              let-values-proc)
                                         env)))))