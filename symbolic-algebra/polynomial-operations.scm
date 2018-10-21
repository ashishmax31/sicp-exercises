#lang racket/load
(load "utils.scm")
(load "dense-termlist.scm")
(load "sparse-termlist.scm")
(load "rational-numbers.scm")

(define (install-polynomial-package)

    (define (tag contents) (attach-tag 'polynomial contents))

    (define (same-variable? a b) (eq? a b))

    (define (make-polynomial variable terms) (cons variable terms))

    (define (variable poly) (car poly))

    (define (term-list poly) (cdr poly))

    (define (empty-termlist) '())

    (define (empty-termlist? term-list) (null? term-list))

    (define (first-term term-list) (cons (car term-list)
                                         (- (length term-list) 1)))

    (define (rest-terms term-list) (cdr term-list))

    (define (empty-polynomial? poly) (null? (term-list poly)))

    (define (get-term-type term) (if (number? (car term)) 'dense 'sparse ))

    (define (coeff item) ((get 'coeff (get-term-type item)) item))

    (define (order item) ((get 'order (get-term-type item)) item))

    (define (adjoin-term term term-list)
        ((get 'adjoin-term (get-term-type term)) term term-list))

    (define (make-term order coefficient type length) (cons ((get 'make-term type) order coefficient) length))


    (define (add-polynomial p1 p2)
        (if (same-variable?  (variable p1) (variable p2))
            (make-polynomial (variable p1)
                             (add-terms (term-list p1)
                                        (term-list p2)))
            (error "ERROR: polynomials should be of the same variable!")))

    (define (sub-polynomial p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-polynomial (variable p1)
                             (sub-terms (term-list p1)
                                        (term-list p2)))
            (error "ERROR: polynomials should be of the same variable!")))


    (define (mul-polynomial p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-polynomial (variable p1)
                             (mul-terms (term-list p1)
                                        (term-list p2)))
            (error "ERROR: polynomials should be of the same variable!")))

    (define (div-polynomial p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (let ((result (div-terms (term-list p1) (term-list p2))))
                (list (make-polynomial (variable p1) (car result)) (make-polynomial (variable p1) (cadr result))))
            (error "ERROR: polynomials should be of the same variable!")))



    (define (add-terms l1 l2)
        (cond ((empty-termlist? l1) l2)
              ((empty-termlist? l2) l1)
              (else (let ((t1 (first-term l1)) (t2 (first-term l2)))
                        (cond  ((> (order t1) (order t2)) (adjoin-term t1
                                                                      (add-terms (rest-terms l1) l2)))
                               ((< (order t1) (order t2)) (adjoin-term t2
                                                                      (add-terms l1 (rest-terms l2))))
                               (else
                                     (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)) (get-term-type t1)  (order t1))
                                                  (add-terms (rest-terms l1) (rest-terms l2)))))))))

    (define (div-terms l1 l2)
        (if (empty-termlist? l1)
            (list (empty-termlist) (empty-termlist))
                  (let ((t1 (first-term l1)) (t2 (first-term l2)))
                        (if (> (order t2) (order t1))
                            (list (empty-termlist) l1)
                            (let ((new-c (div (coeff t1) (coeff t2))) (new-o (sub (order t1) (order t2))))
                                 (let ((quotient-term (cons (list new-o new-c) new-o)))
                                       (let ((new-dividend (sub-terms l1 (mul-with-all-terms quotient-term l2))))
                                            (let ((rest-of-results (list (cons (car quotient-term) (car (div-terms new-dividend l2)))
                                                                         (cadr (div-terms new-dividend l2)))))
                                                  rest-of-results))))))))


    (define (sub-terms l1 l2)
        (add-terms l1 (negate l2)))


    (define (negate term-list)
        (if (number? (car term-list))
            (map (lambda (term) (mul -1 term))
                 term-list)
            (map (lambda (term) (list (car term) (mul -1 (cadr term))))
                 term-list)))

    (define (mul-terms l1 l2)
        (if (empty-termlist? l1)
            (empty-termlist)
            (add-terms (mul-with-all-terms (first-term l1) l2)
                       (mul-terms (rest-terms l1) l2))))

    (define (mul-with-all-terms t1 l2)
        (if (empty-termlist? l2)
            (empty-termlist)
            (adjoin-term (make-term (+ (order t1) (order (first-term l2)))
                                    (mul (coeff t1) (coeff (first-term l2)))
                                    (get-term-type t1)
                                    (+ (order t1) (order (first-term l2))))
                         (mul-with-all-terms t1 (rest-terms l2)))))

    (put 'make 'polynomial (lambda (variable terms) (tag (make-polynomial variable terms))))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-polynomial p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-polynomial p1 p2))))
    (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-polynomial p1 p2))))
    (put 'div '(polynomial polynomial) (lambda (p1 p2) (let ((result (div-polynomial p1 p2))) (list (tag (car result)) (tag (cadr result))))))

    (put 'zero? '(polynomial) empty-polynomial?)
    (display "Intalled polynomial package...")
    (newline)
)

(define (install-number-arithmetic)
    (define (number-zero? item) (zero? item))
    (put 'add '(number number) +)
    (put 'mul '(number number) *)
    (put 'sub '(number number) -)
    (put 'div '(number number) /)
    (put 'zero? '(number) number-zero?)
    (display "Intalled scheme numbers package...")
    (newline)
)

(install-number-arithmetic)
(install-polynomial-package)
(install-dense-termlist)
(install-sparse-termlist)
(install-rational-numbers)


(define (add z1 z2)
    (apply-generic 'add z1 z2))

(define (div z1 z2)
    (apply-generic 'div z1 z2))

(define (sub z1 z2)
    (apply-generic 'sub z1 z2))

(define (mul z1 z2)
    (apply-generic 'mul z1 z2))
(define (numer r)
    (apply-generic 'numer r))
(define (denom r)
    (apply-generic 'denom r))
(define (make-poly var terms) ((get 'make 'polynomial) var terms))
(define (make-rational x y) ((get 'make 'rational ) x y))
(define (=zero? item) (apply-generic 'zero? item))

(define poly (make-poly 'x '((2 1) (1 2) (0 1))))
(define nested (make-poly 'y (list '(2 1) (list 1 poly) '(0 2))))
(define p (make-poly 'x '(4 3 2 1)))
(define p10 (make-poly 'x '((5 1) (0 -1))))
(define p20 (make-poly 'x '((2 1) (0 -1))))
(define p1 (make-poly 'x '((1 1) (0 1))))
(define p2 (make-poly 'x '((3 1) (0 -1))))
(define p3 (make-poly 'x '((1 1))))
(define p4 (make-poly 'x '((2 1) (0 -1))))
(define r1 (make-rational p1 p2))
(define r2 (make-rational p3 p4))