; constructors
(define (make-queue) (cons '() '()))



; Internal selectors
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (empty-queue? queue) (null? (front-ptr queue)))

; Internal mutators

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))


; External procedures

(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "Front queue procedure called for an empty queue!")
        (car (front-ptr queue))))


(define (insert-queue! queue item)
    (let ((new-item (cons item '())))
         (cond ((empty-queue? queue)
               (set-front-ptr! queue new-item)
               (set-rear-ptr! queue new-item)
               queue)
               (else 
                    (set-cdr! (rear-ptr queue) new-item)
                    (set-rear-ptr! queue new-item)
                    queue))))


(define (delete-queue! queue)
    (if (empty-queue? queue)
        (error "Delete queue called on an empty queue!")
        (let ((next-item-after-the-first (cdr (front-ptr queue))))
             (set-front-ptr! queue next-item-after-the-first))))

