#lang scheme


; Recursive process
(define (fringe tree)
  (if (null? tree)
      null
      (if (pair? (car tree))
          (append (fringe (car tree)) (fringe (cdr tree)))
          (append (list (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x (list x x (list x (list x))) x (list x x)))



; Iterative process
(define (fringe tree)
    (define (fringe-iter tree acc)
        (cond ((null? tree) acc)
              ((not (pair? (car tree))) (fringe-iter (cdr tree) (append acc (list (car tree)))))
              (else (fringe-iter (cdr tree) (append acc (fringe-iter (car tree) null))))))
    (fringe-iter tree null))



