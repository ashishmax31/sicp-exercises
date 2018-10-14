
(define (apply-generic op . args)
    (let ((tags (map get-tag args)))
        (let ((proc (get op tags)))
            (if (not (null? proc))
                (apply proc (map contents args))
                (error "Action Dispatch error! no operator found for the type")))))


(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (attach-tag tag contents)
    (cons tag contents))

(define (get-tag datum)
    (car datum))

(define (contents datum)
    (cdr datum))

(define (square x)
    (* x x))