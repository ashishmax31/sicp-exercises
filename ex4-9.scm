;(while (< i 10)
;       (display "hello world")
;       (newline)
;       (set! i (+ i 1))
;       )

;-> (if (< i 10)
;       (begin <while body> (while-expression))
;       'done)


(define (while-expression? expression)
  (tagged-list? expression 'while))

(define (while-expressions while-exp)
  (rest while-exp))

(define (while-condition exps)
  (first exps))

(define (while-body exps)
  (rest exps))

(define (eval-while-expression while-exps)
  (make-if (while-condition while-exps)
           (append (make-begin-sequence (while-body while-exps))
                   (list (cons 'while
                         	   while-exps)))
           '(quoted done)))


;(while (< i 10)
;       (display "hello world")
;       (newline)
;       (set! i (+ i 1))
;       )

; Reduced to ->

;(if (< i 10)
;    (begin
;      (display "hello world")
;      (newline)
;      (set! i (+ i 1))
;      (while
;        (< i 10)
;        (display "hello world")
;        (newline)
;        (set! i (+ i 1))))
;    (quoted done))
