#lang scheme
(define (same-parity . items)
    (if (even? (car items))
        (even-elements items null)
        (odd-elements items null)))


(define (even-elements items res)
    (cond ((null? items) res)
          ((even? (car items)) (even-elements (cdr items) (append res (list (car items)))))
          (else (even-elements (cdr items) res))))


(define (odd-elements items res)
    (cond ((null? items) res)
        ((odd? (car items)) (odd-elements (cdr items) (append res (list (car items)))))
        (else (odd-elements (cdr items) res))))


(define (same-parity a . items)
    (define (parity-iter func items)
        (cond ((null? items) null)
              ((func (car items)) (cons (car items) (parity-iter func (cdr items))))
              (else (parity-iter func (cdr items)))))
     (if (even? a)
         (parity-iter even? (cons a items))
         (parity-iter odd? (cons a items))))



(define (same-parity first . rest) 
    (let ((yes? (if (even? first) 
                    even? 
                    odd?))) 
        (define (iter items result) 
        (if (null? items) 
            (reverse result) 
            (iter (cdr items) (if (yes? (car items)) 
                                    (cons (car items) result) 
                                    result)))) 
        (iter rest (list first))))