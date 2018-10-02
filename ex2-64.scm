#lang scheme
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch))

(define (list->tree items)
    (car (partial-tree items (length items))))


(define (partial-tree elmnts n)
    (if (zero? n)
        (cons '() elmnts)
        (let ((left-size (quotient (- n 1) 2)))
             (let ((left-result (partial-tree elmnts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elemnts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elemnts))
                          (right-result (partial-tree (cdr non-left-elemnts) right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-elements (cdr right-result)))
                              (cons (make-tree this-entry left-tree right-tree) remaining-elements))))))))