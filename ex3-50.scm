#lang scheme
(define (force exp)
    (exp))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car stream)
    (car stream))

(define (stream-null? stream) (null? stream))
(define the-empty-stream '())

(define (stream-cdr stream)
    (force (cdr stream)))

(define (stream-map proc . arg-streams)
    (if (stream-null? (car arg-streams))
        the-empty-stream
        (cons-stream  (apply proc (map stream-car arg-streams))
                      (apply stream-map 
                             (cons proc
                                   (map stream-cdr arg-streams))))))

(define (enumerate-interval a b)
    (if (> a b)
        the-empty-stream
        (cons-stream a
                     (enumerate-interval (+ a 1) b))))

(define (stream-ref stream n)
    (if (= n 0)
        (stream-car stream)
        (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-filter proc stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((proc (stream-car stream))
           (cons-stream (stream-car stream)
                        (stream-filter proc (stream-cdr stream))))
          (else (stream-filter proc (stream-cdr stream)))))

(define sum 0)

(define (accum x)
    (set! sum (+ x sum))
    sum)

(define seq (stream-map accum (enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)
