
(define *op-table* (make-hash))

(define (put op type proc)
    (hash-set! *op-table* (list op type) proc))

(define (get op type)
    (hash-ref *op-table* (list op type) '()))

(define (put-coercion from to proc)
    (hash-set! *op-table* (list from to) proc))

(define (get-coercion from to)
    (hash-ref *op-table* (list from to) '()))

(define (attach-tag tag contents)
    (cons tag contents))

(define (get-tag datum)
    (if (number? datum)
        'number
        (car datum)))

(define (contents datum)
    (if (number? datum)
        datum
        (cdr datum)))

(define (square x)
    (* x x))

(define (apply-generic op . args)
  (let ((type-tags (map get-tag args)))
    (display op)
    (newline)
    (display type-tags)
    (newline)
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (apply proc (map contents args))
          (error "Operation not defined for the type!")))))
