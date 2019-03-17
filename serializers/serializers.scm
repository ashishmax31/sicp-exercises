#lang scheme

(define (make-serializer)
    (let ((mutex (make-mutex)))
         (lambda (proc)
            (define (with-mutex . args)
                    (mutex 'acquire )
                    (let ((return-val (apply proc args)))
                          (mutex 'release )
                          return-val))
            with-mutex)))
