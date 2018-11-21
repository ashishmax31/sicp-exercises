(define (member? item items)
    (pair? (member item items)))


(define (has-cycle? items)
    (define (cycle-helper current-item visited-items)
        (cond ((not (pair? current-item)) #f)
              ((null? pair?) #f)
              ((member? current-item visited-items) #t)
              (else (cycle-helper (cdr current-item) (cons current-item visited-items)))))
    (cycle-helper items '()))


(define (last-item items)
    (if (null? (cdr items))
        items
        (last-item (cdr items))))

(define y '(1 2 3))

(define x '(1 2 3))

(set-cdr! (last-item x) x)

(define z x)