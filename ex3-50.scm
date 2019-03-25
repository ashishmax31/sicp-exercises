(define (stream-map proc . arg-streams)
    (if (stream-null? (car arg-streams))
        the-empty-stream
        (cons-stream  (apply proc (map stream-car arg-streams))
                      (apply stream-map 
                             (cons proc
                                   (map stream-cdr arg-streams))))))