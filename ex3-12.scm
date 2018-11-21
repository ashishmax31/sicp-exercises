#lang scheme

(define (append items item)
    (set-cdr! (last-pair items) item)
    items)



(define (last-pair items)
    (if (null? (cdr items))
        items
        (last-pair (cdr items))))



(define (make-cycle items)
    (set-cdr! (last-pair items) items)
    items)


(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))



; class Hello
;     attr_accessor :a
;     def intialize(a)
;         @a = a
;     end

;     def change
;         self.a = -1
;     end
; end
