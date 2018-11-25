#lang scheme


(define (lookup key table)
    (let ((record (assoc key (cdr table))))
         (if record
             (cdr record)
             #f)))



(define (assoc key records)
    (cond ((null? records) #f)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))


(define (insert-item key value table)
    (let ((record (assoc key (cdr table))))
         (if record
             (set-cdr! record value)
             (let ((new-record (cons key value)))
                  (set-cdr! table 
                            (cons new-record (cdr table)))))))

(define (new-table) (cons 'table '()))