
(define (new-pairs s1 s2)
    (cons-stream (list (stream-car s1) (stream-car s2))
                 (interleave 
                            (s-map (lambda (x) (list (stream-car s1) x))
                                   (stream-cdr s2))
                            (new-pairs (stream-cdr s1) s2))))