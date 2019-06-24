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

; Test expression
;'(letrec ((sumtill (lambda (n) (if (= n 0) 0 (+ n (sumtill (- n 1)))))))
;   (sumtill 5))