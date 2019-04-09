
(define (force exp)
    (exp))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memoised-proc (lambda () exp)))))

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

(define (stream-for-each1 proc stream)
  (if (stream-null? stream)
      'done
      (begin (newline) (display stream) (stream-for-each1 proc (stream-cdr stream)))))

(define (display-stream stream)
  (if (stream-null? stream)
      'done
      (stream-for-each1 display stream)))

(define (stream-null? stream) (null? stream))

(define the-empty-stream '())

(define (enumerate-interval a b)
    (if (> a b)
        the-empty-stream
        (cons-stream a
                     (enumerate-interval (+ a 1) b))))
