(define (after-delay delay action)
    (add-to-agenda! (+ delay (current-agenda-time the-agenda))
                    action
                    the-agenda))

(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-action (first-agenda-item the-agenda)))
             (first-action)
             (remove-first-agenda-item! the-agenda)
             (propagate))))



(define (make-agenda) (list 0))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (segments agenda) (cdr agenda))

(define (segment-time segment) (car segment))

(define (segment-queue segment) (cdr segment))

(define (make-time-segment time queue) (cons time queue))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (current-agenda-time agenda) (car agenda))

(define (set-current-time! agenda new-time) (set-car! agenda new-time))

(define (set-segments! agenda new-segments) (set-cdr! agenda new-segments))


(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (car segments)))))

    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
             (insert-queue! q action)
             (make-time-segment time q)))

    (define (add-to-segments! segments)
        (if (equal? time (segment-time (car segments)))
            (insert-queue! (segment-queue (car segments)) 
                           action)
            (let ((rest-segments (cdr segments)))
                 (if (belongs-before? rest-segments)
                     (set-cdr! segments
                               (cons (make-new-time-segment time action)
                                     (cdr segments)))
                     (add-to-segments! rest-segments)))))

        
    (let ((segments (segments agenda)))
         (if (belongs-before? segments)
             (set-segments! agenda
                            (cons (make-new-time-segment time action)
                                  segments))
             (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
         (delete-queue-item! q)
         (if (empty-queue? q)
            (set-segments! agenda
                           (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty!")
        (let ((segment (first-segment agenda)))
             (set-current-time! agenda (segment-time segment))
             (front-queue (segment-queue segment)))))
