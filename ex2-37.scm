#lang scheme


(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))



(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      '()
      (cons   (accumulate op 
                          initial
                          (map   (lambda (x) (car x))
                                  seqs))
              (accumulate-n   op 
                              initial 
                              (map (lambda (x) (cdr x))
                                    seqs)))))
        

(define (dot-product v w)
  (accumulate +
              0
              (accumulate-n *
                            1
                            (list v w))))


(define (matrix*vector m v)
  (map  (lambda (row)
                (accumulate + 
                            0 
                            (map * row v)))
        m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix*matrix m n)
  (let ((other-mat (transpose n)))
       (map (lambda(row)
                   (map (lambda(current-row)(dot-product row current-row))
                        other-mat))
            m)))

                