#lang scheme
(define (queens board-size)
   (define (queen-cols k) 
     (if (= k 0)
         (list empty-board)
         (filter
          (lambda (positions) (safe? k positions))
          (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))



(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))


(define empty-board null)

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))

(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

(define (flatmap op seq)
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

(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) (cons  (car sequence)
                                            (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))