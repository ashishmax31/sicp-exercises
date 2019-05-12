#lang racket/load
(current-directory (build-path (current-directory) "streams"))
(load "steam-constructors&selectors.scm")


(define (pairs s1 s2 weight-proc)
    (cons-stream (list (stream-car s1) (stream-car s2))
                (weighted-merge 
                            (s-map (lambda (x) (list (stream-car s1) x))
                                   (stream-cdr s2))
                            (pairs (stream-cdr s1) (stream-cdr s2) weight-proc)
                            weight-proc)))

(define (weighted-merge s1 s2 weight-proc)
	(let ((s1-pair (stream-car s1))
		  (s2-pair (stream-car s2)))
		 (if (<= (apply weight-proc s1-pair) (apply weight-proc s2-pair))
		 	 (cons-stream s1-pair
		 	 			  (weighted-merge (stream-cdr s1) s2 weight-proc))
		 	 (cons-stream s2-pair
		 	              (weighted-merge s1 (stream-cdr s2) weight-proc)))))

(define (sum a b) (+ a b))

(define ordered-sum-pairs (pairs integers integers sum))

(define (all? . items)
  (if (null? items)
      #t
      (and (car items) (apply all? (cdr items)))))


(define (remainder-non-zero? nums x)
	(apply all? (map (lambda (n) (not (zero? (remainder x n))))
			  nums)))

(define (not-divisible pair)
	(apply all? (map (lambda (x) (remainder-non-zero? '(2 3 5) x))
	                pair)
             ))

(define custom (s-filter (lambda (pair) (not-divisible pair))
						 (pairs integers integers (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j))))))

(define ramanujan-proc (lambda (i j) (+ (* i i i) (* j j j))))

(define potential-r-numbers-stream (pairs integers integers ramanujan-proc))

(define (r-nums stream)
	(if (= (apply ramanujan-proc (stream-car stream)) (apply ramanujan-proc (stream-car (stream-cdr stream))))
		(cons-stream (apply ramanujan-proc (stream-car stream))
					 (r-nums (stream-cdr (stream-cdr stream))))
	    (r-nums (stream-cdr stream))))

(define ramanujan-numbers (r-nums potential-r-numbers-stream ))


(define square-proc (lambda (i j) (+ (* i i) (* j j))))
(define square-pairs (pairs integers integers square-proc))
(define (s-nums stream)
	(if (= (apply square-proc (stream-car stream)) (apply square-proc (stream-car (stream-cdr stream))))
		(cons-stream (apply square-proc (stream-car stream))
					 (s-nums (stream-cdr (stream-cdr stream))))
	    (s-nums (stream-cdr stream))))

(define square-sum (s-nums square-pairs))
