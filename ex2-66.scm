#lang scheme


(define (lookup given-key records)
    (cond ((null? records) false)
          ((equal? given-key 
                   (key (entry records)))
                   (entry records))
          ((< given-key (key (entry records))) (lookup given-key (left-branch records)))
          (else (lookup given-key (right-branch records)))))