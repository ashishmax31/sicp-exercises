(define (all? items)
    (cond ((null? items) #t)
        ((equal? (car items) #t) (all? (cdr items)))
        ((equal? (car items) #f) #f)
        (else (error "all? applies only on #t or #f!"))))

(define (make-table custom-compare-func)
    (define table (list '() '() '()))

    (define (entry node) (car node))
    (define (left node) (cadr node))
    (define (right node) (caddr node))
    (define (make-new-node key value) (list (list key value) '() '()))
    (define (identifer entry) (car entry))
    (define (value entry) (cadr entry))
    (define less-than? custom-compare-func)
    (define (root-node? table) (and (equal? (length table) 3) (null? (entry table)) (null? (left table)) (null? (right table))))

    (define (set-value-of-node-entry! entry value)
        (set-cdr! entry (cons value '())))

    (define (lookup key)
        (let ((resp (lookup-helper key table)))
             (if resp
                 (value resp)
                 resp)))

    (define (table-empty? table)
        (define fields (list entry left right))
        (or (null? table) (all? (map (lambda (field) (null? (field table)))
                                     fields))))

    (define (lookup-helper key current-table)
            (cond ((table-empty? current-table) #f)
                  ((equal? (identifer (entry current-table)) key) (entry current-table))
                  ((less-than? key (identifer (entry current-table))) (lookup-helper key (left current-table)))
                  (else (lookup-helper key (right current-table)))))

    (define (insert key value)
        (let ((resp (lookup-helper key table)))
             (if resp
                 (set-value-of-node-entry! resp value)
                 (begin (insert-helper key value table)
                    (display table)
                    (newline)))))

    (define (set-node! direction key value table)
        (cond ((equal? direction left) (set-car! (cdr table) (make-new-node key value)))
              ((equal? direction right) (set-car! (cddr table) (make-new-node key value)))
              (else (error "Unknown direction function"))))

    (define (recurse-or-insert direction-proc key value table)
        (if (table-empty? (direction-proc table))
            (set-node! direction-proc key value table)
            (insert-helper key value (direction-proc table))))

    (define (insert-helper key value current-table)
        (cond ((root-node? current-table) (set-car! current-table (cons key (cons value '()))))
              ((less-than? key (identifer (entry current-table))) (recurse-or-insert left key value current-table))
              (else (recurse-or-insert right key value current-table))))


    (define (dispatch message)
        (cond ((eq? message 'lookup ) lookup)
              ((eq? message 'insert ) insert)
              (else (error "Unknown method!"))))

    dispatch)

(define table '())

(define (make function)
    (set! table (make-table function)))

(define (compare-symbols a b)
    (define (validate-and-convert . inputs)
        (cond ((all? (map symbol? inputs)) (map symbol->string inputs))
              ((all? (map number? inputs)) inputs)
              (else (error "Type mismatch! Expected all types to be symbols"))))

    (define (compare a b)
        (cond ((number? a) (<= a b))
              ((symbol? a) (string<=? a b))))

    (apply compare (validate-and-convert a b)))

(make compare-symbols)
(define lookup (table 'lookup ))
(define insert (table 'insert ))
