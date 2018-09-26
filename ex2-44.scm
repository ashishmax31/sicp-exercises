#lang scheme

(define (upsplit pict n)
    (if (zero? n)
        pict
        (let ((smaller (upsplit pict (-n 1))))
             (below pict (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let  ((top (beside (tl painter) (tr painter)))
               (buttom (beside (bl painter) (br painter))))
         (below top buttom))))
