#lang scheme

(define (segments-painter segments)
    (lambda (frame)
        (for-each
            (lambda (segment)
                    (draw-line  ((frame-cordinate-map frame) (start-segment segment))
                                ((frame-cordinate-map frame) (end-segment segment))))
            segments)))

(define (draw-frame-outline frame)
    (segments->painter edges) frame)

(define edges 
    (list 
        (make-segment
            (make-vect 0.0 0.0)
            (make-vect 0.0 0.99))
        (make-segment
            (make-vect 0.0 0.0)
            (make-vect 0.99 0.0))
        (make-segment
            (make-vect 0.99 0.0)
            (make-vect 0.99 0.99))
        (make-segment
            (make-vect 0.0 0.99)
            (make-vect 0.99 0.99))))

