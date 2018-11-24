(define (make-dequeue) (cons '() '()))

(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))

(define (make-node value) (cons value (cons '() '())))  
(define (prev-ptr node) (cadr node))
(define (next-ptr node) (cddr node))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-dequeue? queue) (null? (front-ptr queue)))

(define (set-previous-ptr! current-node previous-node) (set-car! (cdr current-node) previous-node))
(define (set-next-ptr! current-node next-node) (set-cdr! (cdr current-node) next-node))

(define (front-insert-dequeue dq item)
    (let ((new-node (make-node item)))
         (cond ((empty-dequeue? dq)
                (set-front-ptr! dq new-node)
                (set-rear-ptr! dq new-node)
                dq)
               (else (set-previous-ptr! new-node (front-ptr dq))
                     (set-next-ptr! (front-ptr dq) new-node)
                     (set-front-ptr! dq new-node)
                     dq))))


(define (rear-insert-dequeue dq item)
    (let ((new-node (make-node item)))
         (cond ((empty-dequeue? dq)
                (set-front-ptr! dq new-node)
                (set-rear-ptr! dq new-node)
                dq)
               (else 
                     (set-previous-ptr! new-node (rear-ptr dq))
                     (set-next-ptr! (rear-ptr dq) new-node)
                     (set-rear-ptr! dq new-node)
                     dq))))


(define (front-delete-dequeue dq)
    (if (empty-dequeue? dq) 
        (error "Delete called for an empty queue")
        (set-front-ptr! dq (prev-ptr (front-ptr dq)))))


(define (rear-delete-dequeue dq)
    (if (empty-dequeue? dq)
        (error "Delete called for an empty queue")
        (set-rear-ptr! dq (prev-ptr (rear-ptr dq)))))


(define (front-dequeue dq) (car (front-ptr dq)))
(define (rear-dequeue dq) (car (rear-ptr dq)))
