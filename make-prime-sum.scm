#lang scheme

(define (create-pairs n)
    (accumulate append
                '()
                (map (lambda (i) (map (lambda (j) (list i j))
                                        (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))

(define (make-prime-sum n)
    (map 
        make-sum-triplets
        (filter
                sum-prime?
                (create-pairs n))))

(define (sum-prime? pair)
    (prime? (list-sum pair)))

(define (filter op sequence)
    (cond   ((null? sequence) '())
            ((op (car sequence)) (cons (car sequence) 
                                       (filter op (cdr sequence))))
            (else (filter op (cdr sequence)))))

(define (make-sum-triplets pair)
    (append pair (list (list-sum pair))))

(define (list-sum sequence)
    (accumulate +
                0
                sequence))

(define (prime? number)
    (define (prime-iter num)
        (if (> num number)
            #t
            (if (or (equal? (gcd number num) 1)
                    (equal? (gcd number num) number))
                    (prime-iter (+ num 1))
                    #f)))
    (prime-iter 2))

(define (gcd a b)
    (if (equal? b 0)
        a
        (gcd b (remainder a b))))