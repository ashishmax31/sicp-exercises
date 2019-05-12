#lang scheme


(define (make-zero-crossings input-stream last-val last-avg)
	(let ((avpt (average last-val (stream-car input-stream))))
		(cons-stream (sign-change-detector avpt last-avg)
					 (make-zero-crossings (stream-cdr input-stream)
					 					  (stream-car input-stream)
					 					  avpt))))