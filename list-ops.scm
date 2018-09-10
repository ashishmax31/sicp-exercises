(define (list-ref items n)
    (if (zero? n)
        (car items)
        (list-ref (cdr items) (- n 1))))


(define (length items)
    (if (null? items)
        0
        (+ (length (cdr items)) 1)))


(define (length-iterative items)
    (define (len-iter items acc)
        (if (null? items)
            acc
            (len-iter (cdr items) (+ acc 1))))
    (len-iter items 0))


; See the beauty of recursion, everything automatically falls into place.
(define (append-1 list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append-1 (cdr list1) list2))))


(define (scale items num)
    (if (null? items)
        null
        (append (list (* (car items) num))
                 (scale (cdr items) num))))



(define (scale-tree tree factor)
    (cond ((null? tree) null)
          ((not (pair? tree)) (* tree factor))
          (else (cons (scale-tree (car tree) factor)
                      (scale-tree (cdr tree) factor)))))


(define (aa z)
    (if (null? z)
        null
        (cons (square (car z))
              (aa (cdr z)))))



(define (map proc items)
    (if (null? items)
        ('())
        (cons
            (proc (car items))
            (map proc (cdr items)))))


; Better understanding

; If one of the items in the list is a sublist(/tree) then apply
; map to the sub list.
(define (scale-tree-1 tree factor)
    (define (scale-proc x) ((if (pair? x)
                                       (map scale-proc x)
                                       (* x factor))))
    (map scale-proc tree))



; Both the approaches are the same.
(define (scale-tree-1 tree factor)
    (define scale-proc (lambda (x) (if (pair? x)
                                        (scale-tree-1 x factor)
                                        (* x factor))))
    (map scale-proc tree))

