
(load "./metacircular-evaluator/evaluator.scm")



(define (cond->if exps)
    (if (and (last-exp? exps) (eq? (caar exps) 'else))
        (make-begin-sequence (cdar exps))
        (make-if (cond-predicate (first exps))
                 (if-alt-builder (make-begin-sequence (cond-actions (first exps)))
                                 (cond-predicate (first exps))
                                 '())
                 (cond->if (rest exps)))))


(define (if-alt-builder proc args res)
  	(list proc args))


(runner '(cond ((assoc (quoted b)
                       (quoted ((a 1) (b 2)))) cadr)
               (else (+ x 10))))

;'(((assoc (quoted b) (quoted ((a 1) (b 1)))) car) (else (+ x 10)))