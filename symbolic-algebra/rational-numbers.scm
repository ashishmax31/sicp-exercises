(define (install-rational-numbers)
(define (add-rat x y)
    (make-rat (add (mul (numer x)
                        (denom y))
                   (mul (numer y)
                        (denom x)))
              (mul (denom x)
                   (denom y))))

(define (sub-rat x y)
    (make-rat (sub (mul (numer x)
                        (denom y))
                   (mul (numer y)
                        (denom x)))
              (mul (denom x)
                   (denom y))))

(define (mul-rat x y)
    (make-rat (mul (numer x)
                   (numer y))
              (mul (denom x)
                   (denom y))))

(define (divide-rat x y)
    (make-rat (mul (numer x)
                   (denom y))
              (mul (denom y)
                   (numer y))))

(define (make-rat x y)(cons x y))

(define (numer x)
      (car x))

(define (denom x)
      (cdr x))


(define (print-rat x)
      (newline)
      (display (numer x))
      (display "/")
      (display (denom x)))



(define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
(define (tag contents) (attach-tag 'rational contents))
(put 'add '(rational rational) (lambda (r1 r2)(tag (add-rat r1 r2))))
(put 'sub '(rational rational) (lambda (r1 r2)(tag (sub-rat r1 r2))))
(put 'mul '(rational rational) (lambda (r1 r2)(tag (mul-rat r1 r2))))
(put 'div '(rational rational) (lambda (r1 r2)(tag (div-rat r1 r2))))
(put 'numer '(rational) numer)
(put 'denom '(rational) denom)
(put 'make 'rational (lambda(x y) (tag (make-rat x y))))
)