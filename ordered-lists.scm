#lang scheme

(define (element-of-set? set item)
    (cond ((null? set) false)
          ((equal? item (car set)) true)
          ((< item (car set)) false)
          (else (element-of-set? (cdr set) item))))
          
          
(define (set-intersection set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1))
            (x2 (car set2)))
            (cond ((or (null? set1) (null? set2)) '())
                    ((equal? x1 x2) (cons x1 
                                            (set-intersection (cdr set1)
                                                            (cdr set2))))
                    ((< x1 x2) (set-intersection (cdr set1)
                                                set2))
                    ((< x2 x1) (set-intersection set1
                                                (cdr set2)))))))
                
        
   