(define (highest-in-tower args)
    (let ((argumenet-raise-map (map (lambda (arg) (list (count-successfull-raises arg) arg))
                                    args)))
         (cadr (minimum argumenet-raise-map
                        car))))


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
