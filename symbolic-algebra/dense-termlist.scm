(define (install-dense-termlist)
    (define (dense-make-term order coefficient) coefficient)
    (define (dense-order term) (cdr term))
    (define (dense-coeff term) (car term))
    (define (dense-adjoin-term term term-list)
        (define (adjoin-helper t t-list)
            (cond ((< (length t-list) (dense-order t)) (adjoin-helper t (cons 0 t-list)))
                  ((= (length t-list) (dense-order t)) (cons (dense-coeff t) t-list))
                  ((> (length t-list) (dense-order t)) (replace (dense-order t) (dense-coeff t) t-list))))
        (adjoin-helper term term-list))
                

    (define (replace index with items)
        (define (replace-helper current-ind replace-ind with items)
            (cond ((null? items) '())
                  ((= current-ind replace-ind) (cons with
                                                     (replace-helper (- current-ind 1) replace-ind with (cdr items))))
                  (else (cons (car items)
                              (replace-helper (- current-ind 1) replace-ind with (cdr items))))))

        (replace-helper (length items) (+ index 1) with items))
            
    (put 'order 'dense dense-order)
    (put 'coeff 'dense dense-coeff)
    (put 'adjoin-term 'dense dense-adjoin-term)
    (put 'make-term 'dense dense-make-term)
)