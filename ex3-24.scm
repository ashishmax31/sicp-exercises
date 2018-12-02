(define (make-table same-key?)
    ; Internal state
    (define table (cons 'table '()))

    (define (lookup . keys)
        (if (null? keys)
            (error "Expected atlease one key!")
            (cdr (look-up-helper keys table))))

    (define (last-key? keys)
        (null? (cdr keys)))

    (define (subtable? record)
        (and (list? (cdr record)) (not (pair? (car (cdr record))))))

    (define (look-up-helper keys current-table)
        (let ((result (assoc (car keys) (cdr current-table))))
             (cond ((equal? result #f) (cons #f #f))
                   ((subtable? result) (subtable-lookup (cdr keys) (cdr result)))
                   (else result))))


    (define (subtable-lookup keys res)
        (cond ((last-key? keys) (assoc (car keys) res))
              ((same-key? (car keys) (car res)) (subtable-lookup (cdr keys) (cdr res)))
              (else (cons #f #f))))

    (define (last-subtable? keys)
        (equal? (length keys) 2))

    (define (find-last-subtable keys records)
        (if (last-subtable? keys)
            (cons (cdr keys) records)
            (find-last-subtable (cdr keys) (cdr records))))

    (define (last-subtable-key-pair-for-insert keys current-table)
        (let ((record (assoc (car keys) (cdr current-table))))
                (cond ((equal? record #f) (cons keys current-table))
                      ((subtable? record) (find-last-subtable (cdr keys) (cdr record))))))

    (define (assoc key records)
        (cond ((null? records) #f)
              ((same-key? key (caar records)) (car records))
              (else (assoc key (cdr records)))))

    (define (insert value . keys)
        (let ((resp (look-up-helper keys table)))
             (if (and resp (car resp))
                 (set-cdr! resp value)
                 (insert-helper value keys table))))

    (define (insert-helper value keys current-table)
        (let ((concerned-key-table-pair (last-subtable-key-pair-for-insert keys current-table)))
             (let ((concerned-table (cdr concerned-key-table-pair))
                   (concerned-keys (car concerned-key-table-pair)))
                   (set-cdr! concerned-table
                              (cons (if (> (length concerned-keys) 1) (generate-nested-cons-structure concerned-keys value) (cons (car concerned-keys) value))
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
(define new-l (table-ops 'test ))

(define (show message)
    (display message))

(define (assert cond context)
    (if cond
        (begin (show context) (show " Success") (newline))
        (begin (show context) (error "Expectation failed"))))


(insert 100 'a 'b 'c 'k1)
(assert (equal? 100 (lookup 'a 'b 'c 'k1 )) "insert 4 keys")
(insert 200 'a 'b 'c 'k1)
(assert (equal? 200 (lookup 'a 'b 'c 'k1 )) "insert 4 keys")
(insert 1 'x)
(assert (equal? 1 (lookup 'x)) "insert 1 key")
(insert 2 'x)
(assert (equal? 2 (lookup 'x)) "insert 1 key")


