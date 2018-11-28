(define (make-table same-key?)
    ; Internal state
    (define table (cons 'table '()))

    (define e-table (cons 'table
                          (cons (cons 'a 
                                      (cons 'b
                                            (cons (cons 'k1 5)
                                                  '())))
                                '())))

    (define (lookup . keys)
        (if (null? keys)
            (error "Expected atlease one key!")
            (cdr (look-up-helper keys table))))

    (define (lookup-new . keys)
        (if (null? keys)
            (error "Expected atlease one key!")
            (cdr (look-up-helper keys e-table))))    


    (define (last-key? keys)
        (null? (cdr keys)))

    (define (subtable? record)
        (not (pair? (car (cdr record)))))

    (define (key-present-in-subtable? key contents)
        (cond ((null? contents) #f)
              ((pair? (car contents)) (if (assoc key contents) #t #f))
              ((same-key? key (car contents)) #t)
              (else key-present-in-subtable? key (cdr contents))))

    (define (look-up-helper keys current-table)
        (let ((result (assoc (car keys) (cdr current-table))))
             (cond ((equal? result #f) (cons #f #f))
                   ((subtable? result) (subtable-lookup (cdr keys) (cdr result)))
                   (else result))))


    (define (subtable-lookup keys res)
        (cond ((last-key? keys) (assoc (car keys) res))
              ((same-key? (car keys) (car res)) (subtable-lookup (cdr keys) (cdr res)))
              (else (cons #f #f))))


    (define (last-subtable-key-pair-for-insert keys current-table)
        (let ((record (assoc (car keys) (cdr current-table))))
                (if record
                    (last-subtable-key-pair-for-insert (cdr keys) record)
                    (cons keys current-table))))


    (define (assoc key records)
        (cond ((null? records) #f)
              ((same-key? key (caar records)) (car records))
              (else (assoc key (cdr records)))))


    (define (select-subtable key records)
        (cond ((null? records) #f)
              ((same-key? key (car records)) (cdr records))
              (else (select-subtable key (cdr records)))))
    
    (define (insert value . keys)
        (let ((resp (look-up-helper keys table)))
             (display resp)
             (if (car resp)
                 (set-cdr! resp value)
                 (insert-helper value keys table))))

    (define (insert-helper value keys current-table)
        (let ((concerned-key-table-pair (last-subtable-key-pair-for-insert keys current-table)))
             (let ((concerned-table (cdr concerned-key-table-pair))
                   (concerned-keys (car concerned-key-table-pair)))
                   (set-cdr! concerned-table
                              (cons (if (> (length keys) 1) (generate-nested-cons-structure concerned-keys value) (cons (car keys) value))
                                    (cdr concerned-table))))))



    (define (generate-nested-cons-structure keys value)
        (if (null? (cdr keys))
            (cons (cons (car keys) value) '())
            (cons (car keys)
                  (generate-nested-cons-structure  (cdr keys) value))))

    (define (dispatch message)
        (cond ((eq? message 'insert ) insert)
              ((eq? message 'lookup ) lookup)
              ((eq? message 'test ) lookup-new)
              (else (error "Unknown operation"))))

    dispatch)


(define table-ops (make-table equal?))
(define insert (table-ops 'insert ))
(define lookup (table-ops 'lookup ))
(define new-l (table-ops 'test))

(define (show message)
    (display message))

(define (assert cond context)
    (if cond
        (begin (show context) (show " Success") (newline))
        (begin (show context) (error "Expectation failed"))))


; Tests

