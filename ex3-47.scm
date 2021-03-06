; Semaphore implementation.

; a) In terms of mutexes
(define (make-semaphore n)
    (let ((count n)
          (mutex (make-mutex)))
         (define (semaphore message)
            (cond ((eq? message 'acquire ) 
                   (mutex 'acquire )
                   (if (zero? count)
                       (begin (mutex 'release ) (semaphore 'acquire ))
                       (begin (set! count (- count 1)) (mutex 'release ))))
                  ((eq? message 'release )
                   (mutex 'acquire )
                   (if (= count n)
                       (mutex 'release ) 
                       (begin (set! (count (+ count 1))) (mutex 'release ))))))
         semaphore))

(define (make-semaphore n)
    (let ((count n)
          (cell (list #f)))
        (define (semaphore message)
            (cond ((eq? message 'acquire )
                   (if (test-and-set! cell)
                       (semaphore 'acquire )
                       (if (zero? count)
                           (begin (clear! cell) (semaphore 'acquire ))
                           (begin (set! count (- count 1)) (clear! cell)))))
                  ((eq? message 'release )
                   (if (test-and-set! cell)
                       (semaphore 'release )
                       (begin (set! count (+ count 1)) (clear! cell))))))))