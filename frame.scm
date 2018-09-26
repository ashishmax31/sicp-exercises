#lang scheme

(define (make-frame-coord-map frame)
    (lambda(v)
        (add-vect
                 (origin-frame frame)
                 (add-vect (scale-vect (x-cord v) (frame-edge-1 frame))
                           (scale-vect (y-cord v) (frame-edge-2 frame))))))

