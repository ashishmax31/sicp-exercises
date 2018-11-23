(define (make-queue)
    (define front-ptr '())
    (define rear-ptr '())
    (define (empty-queue?) (null? front-ptr))

    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))


    (define (insert-queue! item)
        (let ((new-pair (cons item '())))
             (cond ((empty-queue?)
                    (set-front-ptr! new-pair)
                    (set-rear-ptr! new-pair)
                    dispatch)
                   (else 
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                        dispatch))))

    (define (delete-queue!)
        (if (empty-queue?)
            (error "Delete queue called on an empty queue!")
            (set-front-ptr! (cdr front-ptr))))

    (define (front-queue)
        (if (empty-queue?)
            (error "front queue called on an empty queue!")
            (car front-ptr)))

    (define (dispatch message)
        (cond ((eq? message 'insert-queue! ) insert-queue! )
              ((eq? message 'delete-queue! ) (delete-queue!))
              ((eq? message 'front-queue ) (front-queue))
              (else (error "Unknown operation!"))))
    
    dispatch)


(define (insert-queue! item queue) ((queue 'insert-queue! ) item ))
(define (delete-queue! queue) (queue 'delete-queue! ))
(define (front-queue queue) (queue 'front-queue ))

