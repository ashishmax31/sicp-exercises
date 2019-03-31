
(define (force exp)
    (exp))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

(define (memoised-proc proc)
    (let ((result #f)
          (already-run #f))
         (define (return-proc)
            (if already-run
                result
                (begin (set! result (proc)) (set! already-run #t) result)))
         return-proc))

