(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
         (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (cdr record)
                      #f))
              #f)))



(define (insert-item key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
         (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (set-cdr! record value)
                      (set-cdr! subtable (cons (cons key-2 value)
                                               (cdr subtable)))))
             (set-cdr! table
                       (cons (cons key-1 (cons (cons key-2 value) 
                                               '()))
                             (cdr table))))))

(define (make-table) (cons 'table '()))


(define (assoc key records)
    (cond ((null? records) #f)
          ((equal? (caar records) key) (car record))
          (else (assoc key (cdr records)))))