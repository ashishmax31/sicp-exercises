#lang scheme


(define (unique-pairs n)
    (flat-map
            (lambda (i)(map
                            (lambda(j) (list i j))
                            (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (flat-map op seq)
    (accumulate append
                '()
                (map op seq)))


(define (accumulate op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (accumulate op initial (cdr seq)))))


(define (enumerate-interval start end)
    (if (> start end)
        '()
        (cons start
              (enumerate-interval (+ start 1) end))))
