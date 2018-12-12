(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures))
               (call-each (cdr procedures)))))

(define (get-signal wire)
    (wire 'get-signal ))

(define (set-signal! wire new-value)
    ((wire 'set-signal! ) new-value))

(define (add-action! wire action)
    ((wire 'add-action! ) action))


(define (probe name wire)
    (add-action! wire
                (lambda ()
                    (newline)
                    (display name)
                    (display " ")
                    (display (current-agenda-time the-agenda))
                    (display " new-value = ")
                    (display (get-signal wire)))))


(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (set-front-ptr! queue item)
    (set-car! queue item))
    
(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (insert-queue! queue item)
    (let ((new-item (cons item '())))
        (if (empty-queue? queue)
            (begin (set-front-ptr! queue new-item)
                   (set-rear-ptr! queue new-item))
            (begin (set-cdr! (rear-ptr queue) new-item)
                   (set-rear-ptr! queue new-item)))))


(define (delete-queue-item! queue)
    (if (empty-queue? queue)
        (error "Cannot remove items from an empty queue!")
        (set-front-ptr! queue (cdr (front-ptr queue)))))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "front queue called for an empty queue!")
        (car (front-ptr queue))))