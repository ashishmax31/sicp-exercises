(define (let*-expression? expression) (tagged-list? 'let*))

(define (let*-expressions expression) (cdr expression))

(define (let*-variable-value-list let*-expressions)
  (first let*-expressions))

(define (let*-body let*-expressions)
  (rest let*-expressions))

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