#lang scheme

(define (make-binary-mobile left right)
    (list left right))

(define (make-branch len structure)
    (list len structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

(define (total-weight mobile)
    (cond ((and (not (has-mobile? (left-branch mobile)))
                (not (has-mobile? (right-branch mobile))))
           (+ (left-branch-structure mobile) (right-branch-structure mobile)))
          ((and (not (has-mobile? (left-branch mobile)))
                (has-mobile? (right-branch mobile)))
           (+ (left-branch-structure mobile) (total-weight (right-branch-structure mobile))))
          ((and (has-mobile? (left-branch mobile))
                (not (has-mobile? (right-branch mobile))))
           (+ (right-branch-structure mobile) (total-weight (left-branch-structure mobile))))
          (else (+ (total-weight (left-branch-structure mobile))
                   (total-weight (right-branch-structure mobile))))))

(define (has-mobile? branch)
    (pair? (branch-structure branch)))

(define (left-branch-structure mobile)
    (branch-structure (left-branch mobile)))

(define (right-branch-structure mobile)
    (branch-structure (right-branch mobile)))

(define (branch-torque branch)
    (torque (branch-length branch)
            (branch-structure branch)))

(define (torque len weight)
    (* len weight))

(define (balanced? mobile)
    (cond ((and (not (has-mobile? (left-branch mobile)))
                (not (has-mobile? (right-branch mobile))))
           (equal? (branch-torque (left-branch mobile))
                   (branch-torque (right-branch mobile))))

           ((and (not (has-mobile? (left-branch mobile)))
                 (has-mobile? (right-branch mobile)))
            (and (equal? (branch-torque (left-branch mobile))
                         (* (branch-length (right-branch mobile))
                            (total-weight (right-branch-structure mobile))))
                 (balanced? (right-branch-structure mobile))))

           ((and (has-mobile? (left-branch mobile))
                 (not (has-mobile? (right-branch mobile))))
            (and (equal? (branch-torque (right-branch mobile))
                         (* (branch-length (left-branch mobile))
                            (total-weight (left-branch-structure mobile))))
                 (balanced? (left-branch-structure mobile))))
           (else (and (nested-mobile-torque-equivalence? mobile)
                      (balanced? (left-branch-structure mobile))
                      (balanced? (right-branch-structure mobile))))))


(define (nested-mobile-torque-equivalence? nested-mobile)
    (equal? (torque (branch-length (left-branch nested-mobile))
                    (total-weight (left-branch-structure nested-mobile)))
            (torque (branch-length (right-branch nested-mobile))
                    (total-weight (right-branch-structure nested-mobile)))))








(define z (make-binary-mobile (make-branch 1 10) (make-branch 2 12)))
(define y (make-binary-mobile (make-branch 3 z ) (make-branch 7 z)))
(define x (make-binary-mobile (make-branch 3 y ) (make-branch 7 1)))
