; Named let implementation

; '(let <procedure-name>
;       (bindings)
;       <body>)


;(define (fib n)
;  (let fib-iter ((a 1)
;                 (b 0)
;                 (count n))
;    (if (= count 0)
;        b
;        (fib-iter (+ a b) a (- count 1))))

; Convert it into ->

;(begin (define <procedure-name> (make-lambda <body>))
;  (procedure-name <values>))

(define (make-define variable-name body)
  (list 'define variable-name body))


(define (let-expression? exp) (tagged-list? exp 'let))
(define (named-let? exps) (symbol? (car exps)))

; Let expressions
(define (let-expressions exps) (cdr exps))

(define (let-parameters exps)
  (if (named-let? exps)
      (named-let-parameters exps)
	  (map car
	       (first exps))))

(define (let-body exps)
  (if (named-let? exps)
      (caddr exps)
      (cdr exps)))

(define (let-values exps)
  (if (named-let? exps)
      (named-let-values exps)
	  (map cadr
	       (first exps))))

(define (named-let-parameters exps)
  (map car
       (cadr exps)))

(define (named-let-values exps)
  (map cadr
       (cadr exps)))

(define (named-let-proc-name exps)
  (first exps))

(define (named-let->combination let-exps env)
  (list 'begin
        (make-define (named-let-proc-name let-exps)
                     (make-lambda (let-parameters let-exps)
                                  (let-body let-exps)
                                  env))
        (cons (named-let-proc-name let-exps)
              (list-of-values (let-values let-exps)
                              env))))

(define (let->combination let-exps env)
  (if (named-let? let-exps)
      (named-let->combination let-exps env)
      (cons (make-lambda (let-parameters let-exps)
                         (let-body let-exps)
                         env)
            (list-of-values (let-values let-exps)
                            env))))

