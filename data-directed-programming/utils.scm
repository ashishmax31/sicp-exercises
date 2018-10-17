(define (raise item)
    (apply-generic 'raise item))

(define (same? items item)
    (if (null? items)
        #t
        (and (eq? (car items) item) (same? (cdr items) item))))

(define (present? item)
    (not (null? item)))

(define (transform to contents)
    (map (lambda (from) (list to from (get-coercion from to)))
         (reject to contents)))

(define (flatten-result res)
    (car res))

; (define (apply-generic op . args)
;     (let ((type-tags (map get-tag args)))
;          (let ((proc (get op type-tags)))
;               (if (not (null? proc))
;                   (apply proc (map contents args))
;                   (if (and (>= (length args) 2) (not (same? type-tags (car type-tags))))
;                       (let ((payload (map contents args))
;                             (possible-transformations (map (lambda (to) (transform to type-tags))
;                                                            type-tags)))
;                            (let ((available-transformations (filter all? possible-transformations)))
;                                 (if (null? available-transformations)
;                                     (error "ActionDispatch error cant coerce types.")
;                                     (let ((selected-transformations (map (lambda (item) (cdr item))
;                                                                          (flatten-result available-transformations)))
;                                           (transform-to (car (car available-transformations))))
;                                             (let ((transformed-args (apply-transformations selected-transformations
;                                                                                            args)))
;                                                   (apply apply-generic op transformed-args))))))
;                       (error "ActionDispatch error cant coerce types."))))))


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
    (car datum))

(define (contents datum)
    (cdr datum))

(define (square x)
    (* x x))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append
                '()
                (map proc seq)))

(define (all? items)
    (if (null? items)
        #t
        (and (not (null? (caddr (car items))))
             (all? (cdr items)))))


(define (filter predicate items)
    (cond ((null? items) '())
          ((predicate (car items)) (cons (car items)
                                           (filter predicate (cdr items))))
          (else (filter predicate (cdr items)))))


(define (reject item items)
    (cond ((null? items) '())
          ((equal? (car items) item) (reject item (cdr items)))
          (else (cons (car items)
                      (reject item (cdr items))))))

(define (apply-transformations transformations args)
    (if (null? args)
        '()
        (let ((current-arg-tag (get-tag (car args)))
              (current-arg (car args)))
                (let ((tranformation-func (get-transformation transformations current-arg-tag)))
                      (if (null? tranformation-func)
                          (cons current-arg
                                (apply-transformations transformations (cdr args)))
                          (cons (tranformation-func current-arg)
                                (apply-transformations transformations (cdr args))))))))

(define (reject-by-type type args)
    (if (null? args)
        '()
        (let ((arg-type (get-tag (car args))))
            (cond ((eq? arg-type type) (reject-by-type type (cdr args)))
                    (else (cons (car args)
                                (reject-by-type type (cdr args))))))))


;((t2 funct2->t1) (t3 funct3->t1) (t4 funct4->t1))
(define (get-transformation transformations from-type)
    (cond ((null? transformations) '())
          ((eq? from-type (car (car transformations))) (cadr (car transformations)))
          (else (get-transformation (cdr transformations) from-type))))

(define (highest-in-tower args)
    (let ((argument-raise-map (map (lambda (arg) (list (count-successfull-raises arg) arg))
                                    args)))
         (argument (minimum argument-raise-map
                            car))))

; argument-raised-map format :'(count argument)
(define (argument argument-raised-map)
    (cadr argument-raised-map))


(define (count-successfull-raises arg)
    (define (count-helper arg count)
        (if (null? (raise arg))
            count
            (count-helper (raise arg)
                          (+ count 1))))
    (count-helper arg 0))

(define (minimum items proc)
    (define (min-helper items guess)
        (if (null? items)
            guess
            (if (< (proc (car items)) (proc guess))
                (min-helper (cdr items) (car items))
                (min-helper (cdr items) guess))))
    (min-helper items (car items)))

(define (raise-till target-type arg)
    (let ((current-arg-type (get-tag arg)))
         (if (eq? target-type current-arg-type)
             arg
             (raise-till target-type (raise arg)))))


(define (apply-generic op . args)
  (let ((type-tags (map get-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
          (apply proc (map contents args))
          (let ((highest-arg-type (get-tag (highest-in-tower args))))
                (let ((transformed-args (map (lambda(arg) (raise-till highest-arg-type arg))
                                             args)))
                     (apply apply-generic op  transformed-args)))))))
