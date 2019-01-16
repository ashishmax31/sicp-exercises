(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? sum)) (set-value! a2  (- (get-value sum) (get-value a1)) me))
              ((and (has-value? a2) (has-value? sum)) (set-value! a1  (- (get-value sum) (get-value a2)) me))))

    (define (forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'forget-value ) (forget-value))
              (else (error "Unknown operation!"))))
              
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

(define (multiplier a1 a2 product)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2)) (set-value! product (* (get-value a1) (get-value a2)) me))
              ((and (has-value? a1) (has-value? product)) (set-value! a2  (/ (get-value product) (get-value a1)) me))
              ((and (has-value? a2) (has-value? product)) (set-value! a1  (/ (get-value product) (get-value a2)) me))))

    (define (forget-value)
        (forget-value! product me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))

    (define (me request)
        (cond ((eq? request 'new-value-present ) (process-new-value))
              ((eq? request 'inform-about-no-value ) (forget-value))
              (else (error "Unknown operation!"))))
              
    (connect a1 me)
    (connect a2 me)
    (connect product me)
    me)


(define (constant value connector)
    
    (define (me request)
        (error "Unknown operation!"))
        
    (set-value! connector value me)

    (connect connector me)
    me)


(define (inform-about-new-value constraint)
    (constraint 'new-value-present ))

(define (inform-about-no-value constraint)
    (constraint 'inform-about-no-value ))