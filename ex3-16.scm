(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))

(define three-pairs '(a b c))

(count-pairs three-pairs)


(define b-nil (cons 'b '()))
(define four-pairs (cons 'a (cons b-nil b-nil)))
(count-pairs four-pairs)


(define a.a (cons 'a 'a ))
(define a.a-a.a (cons a.a a.a))
(define seven-pairs (cons a.a-a.a a.a-a.a))

(count-pairs seven-pairs)
