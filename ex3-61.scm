#lang racket/load
(current-directory (build-path (current-directory)))

(define (invert-unit-series series)
	(cons-stream 1
				 (scale-stream -1
				 			   (mul-series (stream-cdr series)
				 			   			   (invert-unit-series series)))))