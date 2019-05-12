#lang scheme


(define (make-zero-crossings smoothened-stream last-avg)
	(cons-stream (sign-change-detector (stream-car smoothened-stream) last-avg)
				 (make-zero-crossings (stream-cdr smoothened-stream)
				 					  (stream-car smoothened-stream))))


(define (smooth input-stream)
	(stream-map average
				input-stream
				(stream-cdr input-stream)))