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

(define (for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (newline) (display stream) (for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (if (stream-null? stream)
      'done
      (for-each display stream)))

(define (divisible? a b)
    (zero? (remainder b a)))


(define (integers-starting-from n)
    (cons-stream n
                 (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (sieve stream)
    (cons-stream (stream-car stream)
                 (sieve (stream-filter (lambda (x)
                                               (not (divisible? (stream-car stream)
                                                                x)))
                                       (stream-cdr stream)))))


(define primes (sieve integers))