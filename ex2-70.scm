
(define true #t)
(define false #f)


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

(define (generate-huffman-tree pairs)
    (successive-merge (make-ordered-pairs pairs)))

(define (adjoin-to-set item set)
    (cond ((null? set) (list item))
          ((equal? (car set) item) set)
          ((< (weight item) (weight (car set))) (cons item
                                                      set))
          (else (cons (car set)
                      (adjoin-to-set item (cdr set))))))


(define (make-ordered-pairs pairs)
    (if (null? pairs)
        '()
        (adjoin-to-set (car pairs)
                       (make-ordered-pairs (cdr pairs)))))

(define set (list '(leaf A 4) '(leaf B 3) '(leaf C 1) '(leaf D 2) '(leaf E 1)))

; First two item in the list will always be the ones with the least weights, so always merge them together.
(define (successive-merge ordered-pairs)
    (if (null? (cdr ordered-pairs))
        (car ordered-pairs)
        (successive-merge (adjoin-to-set (make-code-tree (first ordered-pairs)
                                                         (second ordered-pairs))
                                         (remaining ordered-pairs)))))

(define (first ordered-pairs)
    (car ordered-pairs))

(define (second ordered-pairs)
    (if (> (length ordered-pairs) 1)
        (cadr ordered-pairs)
        '()))

(define (remaining ordered-pairs)
    (cddr ordered-pairs))


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


(define song-symbol-frequencies '((leaf a 2) (leaf boom 1) (leaf get 2) (leaf job 2) (leaf na 16) (leaf sha 3) (leaf yip 9) (leaf wah 1)))

(define song-h-tree (generate-huffman-tree song-symbol-frequencies))

(define rock-song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(display (encode rock-song song-h-tree))
(newline)