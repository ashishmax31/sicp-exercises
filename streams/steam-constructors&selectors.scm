(define (delay exp)
    (memoised-proc (lambda () (exp))))

(define (force exp)
    (exp))

(define (cons-stream a b)
    (cons a (delay b)))

(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

(define (memoised-proc proc)
    (let ((result #f)
          (already-run? #f))
         (lambda () 
            (if already-run?
                result
                (begin (set! result (proc)) (set! already-run? #t) result)))))

