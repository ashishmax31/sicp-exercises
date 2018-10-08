
(define true #t)
(define false #f)

(define (encode message h-tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) h-tree)
                (encode (cdr message) h-tree))))

(define (encode-symbol symbol h-tree)
    (if (leaf? h-tree)
        '()
        (let ((direction (choose-branch symbol h-tree)))
             (let ((branch-to-traverse (direction h-tree)))
                  (cond ((equal? direction left-branch) (cons 0
                                                              (encode-symbol symbol branch-to-traverse)))
                        ((equal? direction right-branch) (cons 1
                                                              (encode-symbol symbol branch-to-traverse))))))))


(define (choose-branch symbol tree)
    (cond ((present-in-set? symbol (left-branch tree)) left-branch)
          ((present-in-set? symbol (right-branch tree)) right-branch)
          (else (error "Symbol not present in the huffman tree"))))

(define (present-in-set? symbol tree)
    (define (include? item list)
        (cond ((null? list) false)
              ((eq? item (car list)) true)
              (else (include? item (cdr list)))))
    (include? symbol (symbols tree)))


(define (left-branch h-tree)
    (car h-tree))

(define (right-branch h-tree)
    (cadr h-tree))

(define (leaf? h-tree)
    (eq? (car h-tree) 'leaf))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (symbol-leaf tree)
    (cadr tree))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))


(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (weight tree)
    (if (leaf? tree)
        (leaf-weight tree)
        (cadddr tree)))

(define (leaf-weight tree)
    (caddr tree))


(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))

(define message '(A D A B B C A))


(display (encode message sample-tree))
(newline)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
