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



(define (enumerate-interval a b)
    (if (> a b)
        the-empty-stream
        (cons-stream a
                     (enumerate-interval (+ a 1) b))))



(define (add-streams s1 s2)
    (if (stream-null? s1)
        the-empty-stream
        (cons-stream (+ (stream-car s1)
                        (stream-car s2))
                     (add-streams (stream-cdr s1)
                                  (stream-cdr s2)))))
                            

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (newline) (proc stream) (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (if (stream-null? stream)
      'done
      (stream-for-each display stream)))

(define s (cons-stream 1
                       (add-streams s
                                    s)))
