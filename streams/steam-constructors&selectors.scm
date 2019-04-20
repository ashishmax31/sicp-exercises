
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

(define (s-map proc . arg-streams)
    (if (stream-null? (car arg-streams))
        the-empty-stream
        (cons-stream  (apply proc (map stream-car arg-streams))
                      (apply s-map
                             (cons proc
                                   (map stream-cdr arg-streams))))))

(define (s-ref stream n)
    (if (= n 0)
        (stream-car stream)
        (s-ref (stream-cdr stream) (- n 1))))

(define (stream-filter proc stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((proc (stream-car stream))
           (cons-stream (stream-car stream)
                        (stream-filter proc (stream-cdr stream))))
          (else (stream-filter proc (stream-cdr stream)))))


(define (stream-starting-from n)
    (cons-stream n
                 (stream-starting-from (+ n 1))))

(define integers (stream-starting-from 1))

(define (scale-stream value stream)
    (if (stream-null? stream)
        the-empty-stream
        (cons-stream (* value (stream-car stream))
                     (scale-stream value (stream-cdr stream)))))

(define (add-streams a b)
  (s-map + a b))
